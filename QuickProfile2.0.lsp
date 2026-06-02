;;; ==========================================================================
;;; 快捷地形剖面图生成插件 (终极满血版：支持“从高到低”趋势投影选项)
;;; ==========================================================================

(defun c:QuickProfile2.0 ( / pmEnt pmLayer dgxSS scaleH scaleV basePt dirMode
                          ptList interPts i ent ed eType z minZ maxZ totalDist 
                          facH facV gridStepH gridStepV tStyleChn tStyleEng 
                          oldOcm oldOsnap oldTStyle xOffset yOffset tickPt tStr endXPt 
                          pmPts curD drawPt p1 p2 sStart sEnd inter2d dist dgxPts idx v taxpayersLine
                          pmStart pmEnd reverseFinal minZ_actual maxZ_actual minZ_grid maxZ_grid
                          pn pn1 z0 zend lastTick maxD_axis
                          realStart realEnd cadAng azi aziStr topY pArrStart pArrEnd pArrFin arrowStartX
                          sum1 sum2 cnt1 cnt2 avg1 avg2 maxP newInterPts dirStr deltaZ ratio)
  
  (setq oldOcm (getvar "CMDECHO"))
  (setq oldOsnap (getvar "OSMODE"))
  (setq oldTStyle (getvar "TEXTSTYLE")) 
  (setvar "CMDECHO" 0)
  
  ;; 1. 【防字体报错】：采用 entmake 和通用字体名
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

  ;; 2. 选择剖面线与等高线
  (setq pmEnt (car (entsel "\n请选择剖面线 (Polyline/Line): ")))
  (while (not pmEnt) (setq pmEnt (car (entsel "\n未选中，请重新选择剖面线: "))))
  (setq pmLayer (cdr (assoc 8 (entget pmEnt))))

  (princ (strcat "\n请框选等高线 [已自动屏蔽图层: " pmLayer "]..."))
  (setq dgxSS (ssget (list '(0 . "LINE,LWPOLYLINE,POLYLINE")
                           (cons -4 "<NOT") (cons 8 pmLayer) (cons -4 "NOT>")
                     )
              )
  )
  (if (not dgxSS) (progn (princ "\n[提示] 未选到有效等高线。") (exit)))

  ;; ==========================================================
  ;; 【新增】：投影方向交互选择
  ;; ==========================================================
  (setq dirMode (getint "\n请选择剖面投影方向 [0-从左至右(默认) / 1-从高到低趋势]: "))
  (if (not dirMode) (setq dirMode 0))

  (setq basePt (getpoint "\n请选择剖面图左下角生成基点: "))
  (if (not basePt) (progn (princ "\n[提示] 未指定基点。") (exit)))

  ;; 3. 纯底层顶点解析
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

  (setq totalDist 0.0 idx 0)
  (while (< idx (1- (length pmPts)))
    (setq totalDist (+ totalDist (distance (nth idx pmPts) (nth (1+ idx) pmPts))))
    (setq idx (1+ idx))
  )
  (setq pmStart (car pmPts) pmEnd (last pmPts))

  ;; 4. 原始求交计算 (按剖面线绘制的自然方向)
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
        
        (setq dist 0.0 idx 0) 
        (while (< idx (1- (length pmPts)))
          (setq sStart (nth idx pmPts) sEnd (nth (1+ idx) pmPts))
          (setq j 0)
          (while (< j (1- (length dgxPts)))
            (setq p1 (nth j dgxPts) p2 (nth (1+ j) dgxPts))
            (setq inter2d (inters sStart sEnd p1 p2 t)) 
            (if inter2d
              (progn
                (setq curDist (+ dist (distance sStart inter2d)))
                (setq interPts (cons (list curDist z) interPts))
              )
            )
            (setq j (1+ j))
          )
          (setq dist (+ dist (distance sStart sEnd)))
          (setq idx (1+ idx))
        )
      )
    )
    (setq i (1+ i))
  )

  (setq interPts (vl-sort interPts '(lambda (e1 e2) (< (car e1) (car e2)))))
  (if (null interPts)
    (progn (princ "\n[警告] 未找到交点！请检查等高线标高是否为0，以及是否与剖面线交叉。") (exit))
  )

  ;; ==========================================================
  ;; 5. 【核心】：高程趋势侦测与方向反转判定
  ;; ==========================================================
  (setq reverseFinal nil)
  (if (= dirMode 0)
    ;; 选项 0：左至右 (如果画线时是从东往西画的，则反转)
    (if (> (car pmStart) (car pmEnd)) (setq reverseFinal t))
    
    ;; 选项 1：高到低趋势
    (progn
      (setq sum1 0.0 cnt1 0 sum2 0.0 cnt2 0)
      ;; 计算前半段和后半段的平均高程
      (foreach p interPts
        (if (< (car p) (/ totalDist 2.0))
          (setq sum1 (+ sum1 (cadr p)) cnt1 (1+ cnt1))
          (setq sum2 (+ sum2 (cadr p)) cnt2 (1+ cnt2))
        )
      )
      (setq avg1 (if (> cnt1 0) (/ sum1 cnt1) 0.0))
      (setq avg2 (if (> cnt2 0) (/ sum2 cnt2) 0.0))
      
      (if (< avg1 avg2)
        (setq reverseFinal t) ;; 前低后高 -> 翻转
        (if (= avg1 avg2)
          ;; 若平均值碰巧相等，找绝对最高点位置兜底
          (progn
            (setq maxP (car (vl-sort interPts '(lambda (e1 e2) (> (cadr e1) (cadr e2))))))
            (if (> (car maxP) (/ totalDist 2.0)) (setq reverseFinal t))
          )
        )
      )
    )
  )

  ;; 如果判定需要反转，重新处理交点里程和真实起止点
  (if reverseFinal
    (progn
      (setq newInterPts '())
      (foreach p interPts
        (setq newInterPts (cons (list (- totalDist (car p)) (cadr p)) newInterPts))
      )
      (setq interPts (vl-sort newInterPts '(lambda (e1 e2) (< (car e1) (car e2)))))
      (setq realStart pmEnd realEnd pmStart)
    )
    (progn
      (setq realStart pmStart realEnd pmEnd)
    )
  )

  ;; 6. 计算最终方位角 (基于决定好的真实投影方向)
  (setq cadAng (angle (list (car realStart) (cadr realStart)) (list (car realEnd) (cadr realEnd))))
  (setq azi (- 90.0 (* (/ cadAng pi) 180.0)))
  (if (< azi 0.0) (setq azi (+ azi 360.0)))
  (setq aziStr (strcat (rtos azi 2 0) "%%d"))

  ;; 7. 端点坡度延伸 (填补空隙)
  (if (>= (length interPts) 2)
    (progn
      (setq p1 (nth 0 interPts) p2 (nth 1 interPts))
      (if (> (car p1) 0.001)
        (progn
          (setq z0 (if (not (equal (car p1) (car p2) 1e-4))
                     (- (cadr p1) (* (car p1) (/ (- (cadr p2) (cadr p1)) (- (car p2) (car p1)))))
                     (cadr p1)))
          (setq interPts (cons (list 0.0 z0) interPts))
        )
      )
      (setq pn (last interPts) pn1 (nth (- (length interPts) 2) interPts))
      (if (< (car pn) (- totalDist 0.001))
        (progn
          (setq zend (if (not (equal (car pn) (car pn1) 1e-4))
                       (+ (cadr pn) (* (- totalDist (car pn)) (/ (- (cadr pn) (cadr pn1)) (- (car pn) (car pn1)))))
                       (cadr pn)))
          (setq interPts (append interPts (list (list totalDist zend))))
        )
      )
    )
  )

  ;; ==========================================================
  ;; 8. 【全自动比例尺算法】 (智能匹配 1:1000 或 1:2000)
  ;; ==========================================================
  (setq minZ_actual (cadr (car (vl-sort interPts '(lambda (e1 e2) (< (cadr e1) (cadr e2)))))))
  (setq maxZ_actual (cadr (car (vl-sort interPts '(lambda (e1 e2) (> (cadr e1) (cadr e2)))))))
  
  (setq deltaZ (if (= maxZ_actual minZ_actual) 1.0 (- maxZ_actual minZ_actual)))
  
  (setq scaleH 1000.0 scaleV 1000.0)
  (setq ratio (/ totalDist deltaZ))
  
  (cond
    ((> ratio 2.0) (setq scaleH 2000.0))
    ((< ratio 0.5) (setq scaleV 2000.0))
  )
  
  (setq facH (/ 1000.0 scaleH) facV (/ 1000.0 scaleV))

  ;; ==========================================================
  ;; 9. 【动态自适应间距算法】 
  ;; ==========================================================
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
  (setq gridStepH 
    (cond 
      ((<= totalDist 100.0) 10.0)
      ((<= totalDist 250.0) 25.0)
      ((<= totalDist 500.0) 50.0)
      ((<= totalDist 1000.0) 100.0)
      ((<= totalDist 2000.0) 200.0)
      ((<= totalDist 5000.0) 500.0)
      (t 1000.0)
    )
  )
  
  (setq minZ_grid (* (if (< minZ_actual 0) (fix (- (/ minZ_actual gridStepV) 0.9999)) (fix (/ minZ_actual gridStepV))) gridStepV))
  (setq maxZ_grid (* (if (< maxZ_actual 0) (fix (/ maxZ_actual gridStepV)) (fix (+ (/ maxZ_actual gridStepV) 0.9999))) gridStepV))
  
  (setq minZ (- minZ_grid gridStepV))
  (setq maxZ (+ maxZ_grid gridStepV))
  
  (setq lastTick (* (fix (+ (/ totalDist gridStepH) 1e-6)) gridStepH))
  (setq maxD_axis (+ lastTick gridStepH))

  ;; 底层纯净文字生成引擎
  (defun draw-text (pt txt hgt rot align style / align1 align2)
    (setq align1 0 align2 0)
    (cond 
      ((= align "_TC") (setq align1 1 align2 3))
      ((= align "_BC") (setq align1 1 align2 1))
      ((= align "_MC") (setq align1 1 align2 2))
      ((= align "_MR") (setq align1 2 align2 2))
    )
    (entmake (list '(0 . "TEXT")
                   (cons 10 pt)
                   (cons 11 pt)
                   (cons 40 hgt)
                   (cons 1 txt)
                   (cons 50 (* rot (/ pi 180.0)))
                   (cons 7 style)
                   (cons 72 align1)
                   (cons 73 align2)
             )
    )
  )

  ;; ==========================================================
  ;; 10. 绘图执行
  ;; ==========================================================
  (setvar "OSMODE" 0)

  (setq axisX (- (car basePt) 2.0))

  ;; 绘制左侧高程轴 (严格贴紧 basePt 的 X 坐标)
  (command "_.LINE" basePt (list (car basePt) (+ (cadr basePt) (* (- maxZ minZ) facV))) "")
  (setq curZ minZ)
  (while (<= curZ maxZ)
    (setq yOffset (* (- curZ minZ) facV))
    (setq tickPt (list (car basePt) (+ (cadr basePt) yOffset)))
    (command "_.LINE" tickPt (list (- (car tickPt) 2.0) (cadr tickPt)) "")
    (draw-text (list (- (car tickPt) 3.0) (cadr tickPt)) (rtos curZ 2 1) 2.5 0 "_MR" tStyleEng)
    (setq curZ (+ curZ gridStepV))
  )

  ;; 绘制底部距离轴
  (setq endXPt (list (+ (car basePt) (* maxD_axis facH)) (cadr basePt)))
  (command "_.LINE" basePt endXPt "")
  
  (setq curD 0.0)
  (while (<= curD maxD_axis)
    (setq xOffset (* curD facH))
    (setq tickPt (list (+ (car basePt) xOffset) (cadr basePt)))
    (command "_.LINE" tickPt (list (car tickPt) (- (cadr tickPt) 2.0)) "")
    (draw-text (list (car tickPt) (- (cadr tickPt) 3.0)) (rtos curD 2 0) 2.5 0 "_TC" tStyleEng)
    (setq curD (+ curD gridStepH))
  )
  
  ;; 坐标轴标题
  (draw-text (list (+ (car basePt) (* maxD_axis 0.5 facH)) (- (cadr basePt) 12.0)) "距离 (m)" 3.5 0 "_BC" tStyleChn)
  (draw-text (list (- (car basePt) 15.0) (+ (cadr basePt) (* (- maxZ minZ) 0.5 facV))) "高程 (m)" 3.5 90 "_MC" tStyleChn)

  ;; 顶部小箭头及方位角 (仅此部分向右偏移 2.0 防粘连)
  (setq topY (+ (cadr basePt) (* (- maxZ minZ) facV)))
  (setq arrowStartX (+ (car basePt) 2.0)) 
  
  (setq pArrStart (list arrowStartX topY))
  (setq pArrEnd (list (+ arrowStartX 16.0) topY))
  (setq pArrFin (list (- (car pArrEnd) 3.0) (+ topY 1.5)))
  
  (command "_.LINE" pArrStart pArrEnd "")
  (command "_.LINE" pArrEnd pArrFin "")
  
  (draw-text (list (+ arrowStartX 8.0) (+ topY 1.0)) aziStr 2.5 0 "_BC" tStyleEng)

  ;; 绘制加粗地形线 (严格贴紧 basePt)
  (setq ptList '())
  (foreach pnt interPts
    (setq curDist (car pnt) curZ (cadr pnt))
    (setq drawPt (list (+ (car basePt) (* curDist facH)) (+ (cadr basePt) (* (- curZ minZ) facV))))
    (setq ptList (cons drawPt ptList))
  )
  
  (if (>= (length ptList) 2)
    (progn
      (command "_.PLINE")
      (foreach pnt ptList (command pnt))
      (command "")
      (setq taxpayersLine (entlast))
      (command "_.PEDIT" taxpayersLine "W" 0.4 "")
    )
  )

  (setvar "OSMODE" oldOsnap)
  (setvar "TEXTSTYLE" oldTStyle)
  (setvar "CMDECHO" oldOcm)
  (setq dirStr (if (= dirMode 1) "高到低趋势" "左至右"))
  (princ (strcat "\n[成功] 智能剖面生成完毕！模式: " dirStr " -> 水平 1:" (rtos scaleH 2 0) "，垂直 1:" (rtos scaleV 2 0)))
  (princ)
)