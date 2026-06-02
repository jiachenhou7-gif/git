;;; ==========================================================================
;;; 快捷地形剖面图生成插件 (终极修复版：坐标轴严丝合缝，仅箭头留白，1:2000)
;;; ==========================================================================

(defun c:QuickProfile ( / pmEnt pmLayer dgxSS scaleH scaleV basePt 
                          ptList interPts i ent ed eType z minZ maxZ totalDist 
                          facH facV gridStepH gridStepV tStyleChn tStyleEng 
                          oldOcm oldOsnap xOffset yOffset tickPt tStr endXPt 
                          pmPts curD drawPt p1 p2 sStart sEnd inter2d dist dgxPts idx v taxpayersLine
                          pmStart pmEnd reverseDir minZ_actual maxZ_actual minZ_grid maxZ_grid
                          pn pn1 z0 zend lastTick maxD_axis
                          realStart realEnd cadAng azi aziStr topY pArrStart pArrEnd pArrFin arrowStartX)
  
  (setq oldOcm (getvar "CMDECHO"))
  (setq oldOsnap (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  
  ;; 1. 初始化标准字体样式 (宋体+新罗马)
  (setq tStyleChn "PM_SIMSUN" tStyleEng "PM_TIMES")
  (if (not (tblsearch "STYLE" tStyleChn))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") 
                   (cons 2 tStyleChn) '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0) '(71 . 0) '(42 . 2.5) 
                   '(3 . "simsun.ttc") '(4 . "")))
  )
  (if (not (tblsearch "STYLE" tStyleChn)) (setq tStyleChn (getvar "TEXTSTYLE")))
  (if (not (tblsearch "STYLE" tStyleEng))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") 
                   (cons 2 tStyleEng) '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0) '(71 . 0) '(42 . 2.5) 
                   '(3 . "times.ttf") '(4 . "")))
  )
  (if (not (tblsearch "STYLE" tStyleEng)) (setq tStyleEng (getvar "TEXTSTYLE")))

  ;; 2. 选择剖面线
  (setq pmEnt (car (entsel "\n请选择剖面线 (Polyline/Line): ")))
  (while (not pmEnt) (setq pmEnt (car (entsel "\n未选中，请重新选择剖面线: "))))
  (setq pmLayer (cdr (assoc 8 (entget pmEnt))))

  ;; 3. 框选等高线
  (princ (strcat "\n请框选等高线 [已自动屏蔽图层: " pmLayer "]..."))
  (setq dgxSS (ssget (list '(0 . "LINE,LWPOLYLINE,POLYLINE")
                           (cons -4 "<NOT") (cons 8 pmLayer) (cons -4 "NOT>")
                     )
              )
  )
  (if (not dgxSS) (progn (princ "\n[提示] 未选到有效等高线。") (exit)))

  ;; 4. 比例与基点 (水平默认改为 1:2000)
  (setq scaleH (getreal "\n输入水平比例尺分母 <2000>: "))
  (if (not scaleH) (setq scaleH 2000.0))
  (setq scaleV (getreal "\n输入垂直比例尺分母 <1000>: "))
  (if (not scaleV) (setq scaleV 1000.0))
  (setq basePt (getpoint "\n请选择剖面图左下角生成基点: "))
  (if (not basePt) (progn (princ "\n[提示] 未指定基点。") (exit)))

  ;; 5. 纯底层顶点解析
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
  (setq reverseDir (if (> (car pmStart) (car pmEnd)) t nil))

  ;; ==========================================================
  ;; 【方位角计算】
  ;; ==========================================================
  (setq realStart (if reverseDir pmEnd pmStart))
  (setq realEnd (if reverseDir pmStart pmEnd))
  
  (setq cadAng (angle (list (car realStart) (cadr realStart)) (list (car realEnd) (cadr realEnd))))
  (setq azi (- 90.0 (* (/ cadAng pi) 180.0)))
  (if (< azi 0.0) (setq azi (+ azi 360.0)))
  (setq aziStr (strcat (rtos azi 2 0) "%%d"))

  ;; 6. 求交计算
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
                (if reverseDir (setq curDist (- totalDist curDist)))
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

  ;; 7. 数据排序及端点坡度延伸
  (setq interPts (vl-sort interPts '(lambda (e1 e2) (< (car e1) (car e2)))))
  (if (null interPts)
    (progn 
      (princ "\n[警告] 未找到交点！请检查等高线标高是否为0，以及是否与剖面线交叉。")
      (exit)
    )
  )

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

  ;; 8. 排版参数计算
  (setq minZ_actual (cadr (car (vl-sort interPts '(lambda (e1 e2) (< (cadr e1) (cadr e2)))))))
  (setq maxZ_actual (cadr (car (vl-sort interPts '(lambda (e1 e2) (> (cadr e1) (cadr e2)))))))
  
  ;; 动态高差判断 (大于80m用20m间隔)
  (if (> (- maxZ_actual minZ_actual) 80.0)
    (setq gridStepV 20.0)
    (setq gridStepV 10.0)
  )
  (setq gridStepH 50.0) 
  
  (setq minZ_grid (* (if (< minZ_actual 0) (fix (- (/ minZ_actual gridStepV) 0.9999)) (fix (/ minZ_actual gridStepV))) gridStepV))
  (setq maxZ_grid (* (if (< maxZ_actual 0) (fix (/ maxZ_actual gridStepV)) (fix (+ (/ maxZ_actual gridStepV) 0.9999))) gridStepV))
  
  (setq minZ (- minZ_grid 10.0))
  (setq maxZ (+ maxZ_grid 10.0))

  (setq facH (/ 1000.0 scaleH) facV (/ 1000.0 scaleV))
  
  (setq lastTick (* (fix (+ (/ totalDist gridStepH) 1e-6)) gridStepH))
  (setq maxD_axis (+ lastTick gridStepH))

  ;; 9. 绘图执行
  (setvar "OSMODE" 0)

  ;; ==========================================================
  ;; 【复位】：坐标轴严丝合缝贴紧原点 basePt
  ;; ==========================================================
  
  ;; 绘制左侧高程轴
  (command "_.LINE" basePt (list (car basePt) (+ (cadr basePt) (* (- maxZ minZ) facV))) "")
  (setq curZ minZ)
  (while (<= curZ maxZ)
    (setq yOffset (* (- curZ minZ) facV))
    (setq tickPt (list (car basePt) (+ (cadr basePt) yOffset)))
    (command "_.LINE" tickPt (list (- (car tickPt) 2.0) (cadr tickPt)) "")
    (command "_.TEXT" "S" tStyleEng "J" "MR" (list (- (car tickPt) 3.0) (cadr tickPt)) 2.5 0 (rtos curZ 2 1))
    (setq curZ (+ curZ gridStepV))
  )

  ;; 绘制底部水平轴
  (setq endXPt (list (+ (car basePt) (* maxD_axis facH)) (cadr basePt)))
  (command "_.LINE" basePt endXPt "")
  
  (setq curD 0.0)
  (while (<= curD maxD_axis)
    (setq xOffset (* curD facH))
    (setq tickPt (list (+ (car basePt) xOffset) (cadr basePt)))
    (command "_.LINE" tickPt (list (car tickPt) (- (cadr tickPt) 2.0)) "")
    (setq tStr (rtos curD 2 0)) 
    (command "_.TEXT" "S" tStyleEng "J" "TC" (list (car tickPt) (- (cadr tickPt) 3.0)) 2.5 0 tStr)
    (setq curD (+ curD gridStepH))
  )
  
  ;; 坐标轴标题
  (command "_.TEXT" "S" tStyleChn "J" "BC" (list (+ (car basePt) (* maxD_axis 0.5 facH)) (- (cadr basePt) 12.0)) 3.5 0 "距离 (m)")
  (command "_.TEXT" "S" tStyleChn "J" "MC" (list (- (car basePt) 15.0) (+ (cadr basePt) (* (- maxZ minZ) 0.5 facV))) 3.5 90 "高程 (m)")

  ;; ==========================================================
  ;; 【精确偏移】：仅顶部小箭头和方位角向右侧产生 2.0 单位的空隙
  ;; ==========================================================
  (setq topY (+ (cadr basePt) (* (- maxZ minZ) facV)))
  (setq arrowStartX (+ (car basePt) 2.0)) 
  
  (setq pArrStart (list arrowStartX topY))
  (setq pArrEnd (list (+ arrowStartX 16.0) topY))
  (setq pArrFin (list (- (car pArrEnd) 3.0) (+ topY 1.5)))
  
  (command "_.LINE" pArrStart pArrEnd "")
  (command "_.LINE" pArrEnd pArrFin "")
  
  (command "_.TEXT" "S" tStyleEng "J" "BC" (list (+ arrowStartX 8.0) (+ topY 1.0)) 2.5 0 aziStr)

  ;; ==========================================================
  ;; 绘制加粗地形线 (紧贴坐标轴)
  ;; ==========================================================
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
  (setvar "CMDECHO" oldOcm)
  (princ "\n[成功] 坐标轴贴紧，仅箭头偏移的完美剖面图已生成！")
  (princ)
)