(defun c:ZHFRAC (/ ss i ent obj textStr newStr doc space pt height style layer rot newMtext)
  (vl-load-com)
  ;; 获取当前文档空间
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= (vla-get-ActiveSpace doc) 1)
    (setq space (vla-get-ModelSpace doc))
    (setq space (vla-get-PaperSpace doc))
  )

  (prompt "\n请框选需要转换为分式的文字(支持直接选单行TEXT): ")
  (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    (progn
      (vla-StartUndoMark doc) ; 开启撤销记录，防止出错可以Ctrl+Z一键退回
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq textStr (vla-get-TextString obj))

        ;; 判断是否包含斜杠，并且尚未被转化为分式
        (if (and (vl-string-search "/" textStr)
                 (not (vl-string-search "\\S" (strcase textStr))))
          (progn
            (setq newStr (strcat "\\S" textStr ";"))

            (if (= (vla-get-ObjectName obj) "AcDbText") ; 如果是单行文字
              (progn
                ;; 获取原文字的精确位置和属性
                (setq pt (vla-get-InsertionPoint obj))
                (setq height (vla-get-Height obj))
                (setq style (vla-get-StyleName obj))
                (setq layer (vla-get-Layer obj))
                (setq rot (vla-get-Rotation obj))

                ;; 原地创建一个新的多行文字
                (setq newMtext (vla-AddMText space pt 0 newStr))

                ;; 完美继承原文字的格式
                (vla-put-Height newMtext height)
                (vla-put-StyleName newMtext style)
                (vla-put-Layer newMtext layer)
                (vla-put-Rotation newMtext rot)
                
                ;; 锁定对齐基点，防止位置偏移
                (vla-put-AttachmentPoint newMtext 7) ; 设置为左下对齐
                (vla-put-InsertionPoint newMtext pt) ; 强制吸附回原坐标

                ;; 删除原单行文字
                (vla-Delete obj)
              )
              (progn
                ;; 如果本身已经是多行文字，直接原地更新
                (vla-put-TextString obj newStr)
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (vla-EndUndoMark doc)
      (princ "\n完美转换！所有标注已在原坐标独立生成堆叠分式。")
    )
  )
  (princ)
)