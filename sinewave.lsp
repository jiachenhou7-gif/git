(defun c:SINEFAST (/ ent p1 p2 len ang amp freq seg totalSeg i x y pt)
  (setq ent (car (entsel "\n请选择一条作为基准的直线: ")))
  (if (and ent (= (cdr (assoc 0 (entget ent))) "LINE"))
    (progn
      (setq p1 (cdr (assoc 10 (entget ent)))
            p2 (cdr (assoc 11 (entget ent)))
            len (distance p1 p2)
            ang (angle p1 p2))
      
      ;; ====================================================
      ;; 请在这里填入你调试好的最终参数！
      (setq amp 1)   ; ← 将 10.0 修改为你调试好的“振幅”数值
      (setq freq 3.0)   ; ← 将 5.0 修改为你调试好的“波浪数量”数值
      ;; ====================================================
      
      (setq seg 40) ; 每个周期的线段数量，数值越大曲线越平滑
      (setq totalSeg (fix (* freq seg)))
      
      ;; 临时关闭捕捉，防止画线时捕捉到其他点导致变形
      (setq old_osmode (getvar "OSMODE"))
      (setvar "OSMODE" 0) 
      
      (command "_.PLINE")
      (setq i 0)
      (while (<= i totalSeg)
        (setq x (* len (/ (float i) totalSeg)))
        (setq y (* amp (sin (* 2 pi freq (/ (float i) totalSeg)))))
        (setq pt (polar (polar p1 ang x) (+ ang (/ pi 2)) y))
        (command "_non" pt)
        (setq i (1+ i))
      )
      (command "")
      
      ;; 恢复捕捉设置
      (setvar "OSMODE" old_osmode)
      
      ;; 删除原来的基准直线
      (entdel ent)
      
      (princ "\n完美！正弦线已生成，原直线已自动删除。")
    )
    (princ "\n错误: 您选择的不是一条直线，请重新运行命令。")
  )
  (princ)
)