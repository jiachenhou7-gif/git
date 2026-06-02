;;; ==========================================================================
;;; 长线路标准地形剖面图生成插件 (双向联动终极版)
;;; 横向比例: 1:10000, 纵向比例: 1:2000
;;; 新增：1. 地面高程强制保留2位小数(.00)  2. 可选在原平面图线路上绘制沿线百米桩号
;;; ==========================================================================

(defun c:RouteProfile ( / pmEnt pmLayer dgxSS basePt 
                          userStart pmPts d1 d2 reverseDir totalDist routePts
                          interPts i ent ed eType z p1 p2 sStart sEnd inter2d 
                          dist dgxPts idx v taxpayersLine
                          minZ_actual maxZ_actual minZ maxZ gridStepV gridStepH
                          facH facV stPrefix addMarkMode stList curD stData drawPt
                          tableY1 tableY2 tableY3 headerX endX tickY curZ curX target_minZ
                          pmSegs pA pB minx maxx miny maxy len rMinX rMaxX rMinY rMaxY
                          cMinX cMaxX cMinY cMaxY cx1 cx2 cy1 cy2 rSeg deltaZ
                          oldOcm oldOsnap oldTStyle oldDimZin ptAng p ang perpAng degAng tickP1 tickP2 txtP)
  
  (setq oldOcm (getvar "CMDECHO"))
  (setq oldOsnap (getvar "OSMODE"))
  (setq oldTStyle (getvar "TEXTSTYLE")) 
  ;; 【核心更新】：接管尾随零系统变量，确保强制输出 .00
  (setq oldDimZin (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setvar "CMDECHO" 0)
  
  ;; 1. 字体初始化引擎
  (setq tStyleChn "PM_SIMSUN" tStyleEng "PM_TIMES")
  (if (not (tblsearch "STYLE" tStyleChn))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") 
                   (cons 2 tStyleChn) '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0) '(71 . 0) '(42 . 2.5) 
                   '(3 . "宋体") '(4 . "")))
  )
  (if (not (tblsearch "STYLE" tStyleChn)) (setq tStyleChn (getvar "TEXTSTYLE")))
  (if (not (tblsearch "STYLE" tStyleEng))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") 
                   (cons 2 tStyleEng) '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0) '(71 . 0) '(42 . 2.5) 
                   '(3 . "Times New Roman") '(4 . "")))
  )
  (if (not (tblsearch "STYLE" tStyleEng)) (setq tStyleEng (getvar "TEXTSTYLE")))

  ;; 2. 选择线路及起点
  (setq pmEnt (car (entsel "\n[步骤1] 请选择长线路剖面线 (Polyline/Line): ")))
  (while (not pmEnt) (setq pmEnt (car (entsel "\n未选中，请重新选择线路: "))))
  (setq pmLayer (cdr (assoc 8 (entget pmEnt))))
  
  (setq userStart (getpoint "\n[步骤2] 请在图上点击该线路的【起始端点】(决定桩号0点): "))
  (while (not userStart) (setq userStart (getpoint "\n必须指定起始端点: ")))

  ;; 3. 框选区域 (自动过滤杂线)
  (princ (strcat "\n[步骤3] 请框选包含该线路的【整个测区】(已自动屏蔽图层: " pmLayer ")..."))
  (setq dgxSS (ssget (list '(0 . "LINE,LWPOLYLINE,POLYLINE") 
                           (cons -4 "<NOT") (cons 8 pmLayer) (cons -4 "NOT>")
                     )
              )
  )
  (if (not dgxSS) (progn (princ "\n未选到任何线，退出。") (exit)))

  ;; 4. 设置桩号与交互
  (setq stPrefix (getstring "\n[步骤4] 请输入桩号前缀 (如 K, AK，若无直接回车) <K>: "))
  (if (= stPrefix "") (setq stPrefix "K"))
  
  (setq basePt (getpoint "\n[步骤5] 请在空白处点击生成剖面图的左下角基点: "))
  (if (not basePt) (progn (princ "\n未指定基点，退出。") (exit)))
  
  ;; 【新增】：平面布桩开关
  (setq addMarkMode (getint "\n[步骤6] 是否在原平面线路上绘制沿线百米桩号？[0-否(默认) / 1-是]: "))
  (if (not addMarkMode) (setq addMarkMode 0))

  ;; 5. 解析线路顶点
  (defun get-pts (ent / e_d e_type pts e_v)
    (setq e_d (entget ent) e_type (cdr (assoc 0 e_d)) pts '())
    (cond 
      ((= e_type "LINE")
       (list (cdr (assoc 10 e_d)) (cdr (assoc 11 e_d))))
      ((= e_type "LWPOLYLINE")
       (vl-remove-if 'not (mapcar '(lambda (x) (if (= (car x) 10) (cdr x))) e_d)))
      ((= e_type "POLYLINE")
       (setq e_v (entnext ent))
       (while (= (cdr (assoc 0 (entget e_v))) "VERTEX")
         (setq pts (cons (cdr (assoc 10 (entget e_v))) pts))
         (setq e_v (entnext e_v))
       )
       (reverse pts)
      )
    )
  )
  
  (setq pmPts (get-pts pmEnt))
  (if (and pmPts (> (length (car pmPts)) 2))
    (setq pmPts (mapcar '(lambda (p) (list (car p) (cadr p))) pmPts))
  )

  ;; AABB 空间索引预处理
  (setq pmSegs '() idx 0 totalDist 0.0)
  (setq rMinX 1e99 rMaxX -1e99 rMinY 1e99 rMaxY -1e99)
  
  (while (< idx (1- (length pmPts)))
    (setq pA (nth idx pmPts) pB (nth (1+ idx) pmPts))
    (setq minx (min (car pA) (car pB)) maxx (max (car pA) (car pB))
          miny (min (cadr pA) (cadr pB)) maxy (max (cadr pA) (cadr pB))
          len (distance pA pB))
          
    (setq pmSegs (cons (list pA pB minx maxx miny maxy totalDist) pmSegs))
    
    (setq rMinX (min rMinX minx) rMaxX (max rMaxX maxx)
          rMinY (min rMinY miny) rMaxY (max rMaxY maxy))
          
    (setq totalDist (+ totalDist len))
    (setq idx (1+ idx))
  )
  (setq pmSegs (reverse pmSegs))
  
  (setq d1 (distance (list (car userStart) (cadr userStart)) (car pmPts)))
  (setq d2 (distance (list (car userStart) (cadr userStart)) (last pmPts)))
  (setq reverseDir (if (> d1 d2) t nil))
  
  ;; 获取严格按照桩号递增方向排序的节点列表 (供布桩使用)
  (setq routePts (if reverseDir (reverse pmPts) pmPts))

  ;; 6. 求交计算 (AABB 极速引擎)
  (princ "\n正在极速计算交点，请稍候...")
  (setq interPts '() i 0)
  (repeat (sslength dgxSS)
    (setq ent (ssname dgxSS i) ed (entget ent) eType (cdr (assoc 0 ed)) z nil)
    (cond
      ((= eType "LWPOLYLINE") (setq z (cdr (assoc 38 ed))))
      ((= eType "POLYLINE") 
       (setq v (entnext ent))
       (if (= (cdr (assoc 0 (entget v))) "VERTEX") (setq z (caddr (cdr (assoc 10 (entget v))))))
      )
      ((= eType "LINE") (setq z (caddr (cdr (assoc 10 ed)))))
    )
    
    (if (and z (not (equal z 0.0 1e-4)))
      (progn
        (setq dgxPts (get-pts ent))
        (setq dgxPts (mapcar '(lambda (p) (list (car p) (cadr p))) dgxPts))
        
        (setq cMinX 1e99 cMaxX -1e99 cMinY 1e99 cMaxY -1e99)
        (foreach p dgxPts 
          (setq cMinX (min cMinX (car p)) cMaxX (max cMaxX (car p))
                cMinY (min cMinY (cadr p)) cMaxY (max cMaxY (cadr p)))
        )
        
        (if (and (<= rMinX cMaxX) (>= rMaxX cMinX) (<= rMinY cMaxY) (>= rMaxY cMinY))
          (progn
            (setq j 0)
            (while (< j (1- (length dgxPts)))
              (setq p1 (nth j dgxPts) p2 (nth (1+ j) dgxPts))
              (setq cx1 (min (car p1) (car p2)) cx2 (max (car p1) (car p2))
                    cy1 (min (cadr p1) (cadr p2)) cy2 (max (cadr p1) (cadr p2)))
              
              (foreach rSeg pmSegs
                (if (and (<= (nth 2 rSeg) cx2) (>= (nth 3 rSeg) cx1)
                         (<= (nth 4 rSeg) cy2) (>= (nth 5 rSeg) cy1))
                  (progn
                    (setq inter2d (inters (nth 0 rSeg) (nth 1 rSeg) p1 p2 t)) 
                    (if inter2d
                      (progn
                        (setq curDist (+ (nth 6 rSeg) (distance (nth 0 rSeg) inter2d)))
                        (if reverseDir (setq curDist (- totalDist curDist)))
                        (setq interPts (cons (list curDist z) interPts))
                      )
                    )
                  )
                )
              )
              (setq j (1+ j))
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )

  (setq interPts (vl-sort interPts '(lambda (e1 e2) (< (car e1) (car e2)))))
  (if (null interPts)
    (progn (princ "\n[错误] 未找到任何交点！") (setvar "DIMZIN" oldDimZin) (exit))
  )

  (if (> (car (nth 0 interPts)) 0.001)
    (setq interPts (cons (list 0.0 (cadr (nth 0 interPts))) interPts))
  )
  (if (< (car (last interPts)) (- totalDist 0.001))
    (setq interPts (append interPts (list (list totalDist (cadr (last interPts))))))
  )

  ;; ==========================================================
  ;; 7. 比例尺设置与智能留白算法
  ;; ==========================================================
  (setq facH (/ 1000.0 10000.0)) ;; 横向 1:10000
  (setq facV (/ 1000.0 2000.0))  ;; 纵向 1:2000
  
  (setq minZ_actual (cadr (car (vl-sort interPts '(lambda (e1 e2) (< (cadr e1) (cadr e2)))))))
  (setq maxZ_actual (cadr (car (vl-sort interPts '(lambda (e1 e2) (> (cadr e1) (cadr e2)))))))
  
  (setq deltaZ (- maxZ_actual minZ_actual))
  (if (equal deltaZ 0.0 1e-4) (setq deltaZ 1.0))

  (setq gridStepV 
    (cond 
      ((<= deltaZ 20.0) 2.0)
      ((<= deltaZ 50.0) 5.0)
      ((<= deltaZ 100.0) 10.0)
      ((<= deltaZ 200.0) 20.0)
      ((<= deltaZ 500.0) 50.0)
      (t 100.0)
    )
  )
  
  (setq target_minZ (- minZ_actual 100.0))
  (setq minZ (* (if (< target_minZ 0) (fix (- (/ target_minZ gridStepV) 0.9999)) (fix (/ target_minZ gridStepV))) gridStepV))
  (setq maxZ (+ (* (if (< maxZ_actual 0) (fix (/ maxZ_actual gridStepV)) (fix (+ (/ maxZ_actual gridStepV) 0.9999))) gridStepV) gridStepV))

  ;; 8. 智能插值引擎 
  (defun interp-z (d pts / i p1 p2 res)
    (setq i 0 res nil)
    (while (and (< i (1- (length pts))) (not res))
      (setq p1 (nth i pts) p2 (nth (1+ i) pts))
      (if (and (>= d (car p1)) (<= d (car p2)))
        (if (equal (car p1) (car p2) 1e-4)
          (setq res (cadr p1))
          (setq res (+ (cadr p1) (* (- d (car p1)) (/ (- (cadr p2) (cadr p1)) (- (car p2) (car p1))))))
        )
      )
      (setq i (1+ i))
    )
    (if (not res) (setq res (cadr (last pts))))
    res
  )

  (setq stData '() curD 0.0 gridStepH 100.0)
  (while (<= curD totalDist)
    (setq stData (cons (list curD (interp-z curD interPts)) stData))
    (setq curD (+ curD gridStepH))
  )
  (if (not (equal (car (car stData)) totalDist 1e-1))
    (setq stData (cons (list totalDist (interp-z totalDist interPts)) stData))
  )
  (setq stData (reverse stData))

  (defun format-st (pref d / dInt km remM)
    (setq dInt (fix (+ d 0.5)))
    (setq km (/ dInt 1000))
    (setq remM (rem dInt 1000))
    (strcat pref (itoa km) "+" 
      (if (< remM 10) (strcat "00" (itoa remM))
        (if (< remM 100) (strcat "0" (itoa remM))
          (itoa remM)
        )
      )
    )
  )

  ;; 沿线路提取坐标与夹角 (供布桩使用)
  (defun get-pt-and-ang-at-dist (pts d / i p1 p2 segLen curLen ratio pt ang res)
    (setq i 0 curLen 0.0 res nil)
    (while (and (< i (1- (length pts))) (not res))
      (setq p1 (nth i pts) p2 (nth (1+ i) pts))
      (setq segLen (distance p1 p2))
      (if (<= d (+ curLen segLen 1e-4))
        (progn
          (setq ratio (if (= segLen 0) 0.0 (/ (- d curLen) segLen)))
          (setq pt (list (+ (car p1) (* ratio (- (car p2) (car p1))))
                         (+ (cadr p1) (* ratio (- (cadr p2) (cadr p1))))))
          (setq ang (angle p1 p2))
          (setq res (list pt ang))
        )
      )
      (setq curLen (+ curLen segLen))
      (setq i (1+ i))
    )
    (if (not res)
      (setq res (list (last pts) (angle (nth (- (length pts) 2) pts) (last pts))))
    )
    res
  )

  ;; 9. 底层文字引擎
  (defun draw-text (pt txt hgt rot align style / align1 align2)
    (setq align1 0 align2 0)
    (cond 
      ((= align "_TC") (setq align1 1 align2 3))
      ((= align "_BC") (setq align1 1 align2 1))
      ((= align "_MC") (setq align1 1 align2 2))
      ((= align "_MR") (setq align1 2 align2 2))
      ((= align "_ML") (setq align1 0 align2 2))
    )
    (entmake (list '(0 . "TEXT") (cons 10 pt) (cons 11 pt) (cons 40 hgt) (cons 1 txt) 
                   (cons 50 (* rot (/ pi 180.0))) (cons 7 style) (cons 72 align1) (cons 73 align2)))
  )

  ;; ==========================================================
  ;; 10. 绘图执行
  ;; ==========================================================
  (setvar "OSMODE" 0)
  
  ;; 【功能1】：平面布桩逻辑
  (if (= addMarkMode 1)
    (progn
      (princ "\n正在平面图上绘制路线桩号...")
      (foreach st stData
        (setq curD (car st))
        (setq ptAng (get-pt-and-ang-at-dist routePts curD))
        (setq p (car ptAng) ang (cadr ptAng))
        
        ;; 算出法线方向画短线
        (setq perpAng (+ ang (/ pi 2.0)))
        (setq degAng (* perpAng (/ 180.0 pi)))
        (setq tickP1 p)
        (setq tickP2 (polar p perpAng 5.0)) ;; 垂直短线长 5.0 单位
        (command "_.LINE" tickP1 tickP2 "")
        
        ;; 智能旋转文字使其朝向易读
        (if (and (> degAng 90.0) (<= degAng 270.0))
          (progn
            (setq degAng (- degAng 180.0))
            (setq txtP (polar p perpAng 6.0))
            (draw-text txtP (format-st stPrefix curD) 2.5 degAng "_MR" tStyleEng)
          )
          (progn
            (setq txtP (polar p perpAng 6.0))
            (draw-text txtP (format-st stPrefix curD) 2.5 degAng "_ML" tStyleEng)
          )
        )
      )
    )
  )

  ;; 【功能2】：绘制剖面图主结构
  (setq tableY1 (cadr basePt))               
  (setq tableY2 (- tableY1 12.0))            
  (setq tableY3 (- tableY1 26.0))            
  (setq headerX (- (car basePt) 20.0))       
  (setq endX (+ (car basePt) (* totalDist facH)))

  ;; 绘制表格水平外框
  (command "_.LINE" (list headerX tableY1) (list endX tableY1) "")
  (command "_.LINE" (list headerX tableY2) (list endX tableY2) "")
  (command "_.LINE" (list headerX tableY3) (list endX tableY3) "")
  
  (command "_.LINE" (list headerX (+ tableY1 (* (- maxZ minZ) facV))) (list headerX tableY3) "")
  (command "_.LINE" (list (car basePt) tableY1) (list (car basePt) tableY3) "")
  
  (draw-text (list (+ headerX 10.0) (- tableY1 6.0)) "地面高程" 3.0 0 "_MC" tStyleChn)
  (draw-text (list (+ headerX 10.0) (- tableY2 7.0)) "桩    号" 3.0 0 "_MC" tStyleChn)
  
  ;; 左侧高程标尺
  (setq curZ minZ)
  (while (<= curZ maxZ)
    (setq tickY (+ tableY1 (* (- curZ minZ) facV)))
    (command "_.LINE" (list headerX tickY) (list (- headerX 2.0) tickY) "")
    (draw-text (list (- headerX 3.0) tickY) (rtos curZ 2 1) 2.5 0 "_MR" tStyleEng)
    (setq curZ (+ curZ gridStepV))
  )

  ;; 填充百米桩表格数据列
  (foreach st stData
    (setq curD (car st) curZ (cadr st))
    (setq curX (+ (car basePt) (* curD facH)))
    
    (command "_.LINE" (list curX tableY1) (list curX tableY3) "")
    (command "_.LINE" (list curX tableY1) (list curX (+ tableY1 (* (- maxZ minZ) facV))) "")
    (command "_.CHPROP" (entlast) "" "C" "8" "") 

    ;; 由于 DIMZIN 已被设为0，这里强制输出为带有2位小数的格式 (如 100.00)
    (draw-text (list (+ curX 1.5) (- tableY1 6.0)) (rtos curZ 2 2) 2.5 90 "_MC" tStyleEng)
    (draw-text (list (+ curX 1.5) (- tableY2 7.0)) (format-st stPrefix curD) 2.5 90 "_MC" tStyleEng)
  )

  ;; 绘制实际地形折线 
  (setq ptList '())
  (foreach pnt interPts
    (setq drawPt (list (+ (car basePt) (* (car pnt) facH)) (+ tableY1 (* (- (cadr pnt) minZ) facV))))
    (setq ptList (cons drawPt ptList))
  )
  (setq ptList (reverse ptList))
  
  (if (>= (length ptList) 2)
    (progn
      (command "_.PLINE")
      (foreach pnt ptList (command pnt))
      (command "")
      (setq taxpayersLine (entlast))
      (command "_.PEDIT" taxpayersLine "W" 0.4 "")
    )
  )

  (command "_.LINE" (list endX (+ tableY1 (* (- maxZ minZ) facV))) (list endX tableY3) "")
  
  ;; 还原用户的系统变量
  (setvar "OSMODE" oldOsnap)
  (setvar "TEXTSTYLE" oldTStyle)
  (setvar "DIMZIN" oldDimZin)
  (setvar "CMDECHO" oldOcm)
  (princ "\n[成功] 长线路地形剖面生成完毕！(.00高程已就绪)")
  (princ)
)