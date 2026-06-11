(defun c:zhuji (/ ss i ent obj textStr newStr doc space align pt height style layer rot mtxtAttach newMtext)
  (vl-load-com)
  ;; 获取当前文档空间
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= (vla-get-ActiveSpace doc) 1)
    (setq space (vla-get-ModelSpace doc))
    (setq space (vla-get-PaperSpace doc))
  )

  (prompt "\n请框选需要转换为分式的文字: ")
  (if (setq ss (ssget '((0 . "*TEXT"))))
    (progn
      (vla-StartUndoMark doc) ; 开启安全撤销
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq textStr (vla-get-TextString obj))

        ;; ==========================================
        ;; 核心修复区：清理 ArcGIS 导出的多余百分号
        ;; 1. 替换所有的 "%%%" 为 "%"
        (while (vl-string-search "%%%" textStr)
          (setq textStr (vl-string-subst "%" "%%%" textStr))
        )
        ;; 2. 替换挨着斜杠的 "%%/" 为 "%/"
        (while (vl-string-search "%%/" textStr)
          (setq textStr (vl-string-subst "%/" "%%/" textStr))
        )
        ;; 3. 如果整个文本以 "%%" 结尾，清理掉末尾多余的一个 %
        (if (wcmatch textStr "*%%")
          (setq textStr (strcat (substr textStr 1 (- (strlen textStr) 2)) "%"))
        )
        ;; ==========================================

        ;; 检查是否包含斜杠，且尚未被转为分式
        (if (and (vl-string-search "/" textStr)
                 (not (vl-string-search "\\S" (strcase textStr))))
          (progn
            (setq newStr (strcat "\\S" textStr ";"))

            (if (= (vla-get-ObjectName obj) "AcDbText") ; 如果是单行文字
              (progn
                ;; 读取真实的对齐基点
                (setq align (vla-get-Alignment obj))
                (if (or (= align 0) (= align 13) (= align 15))
                    (setq pt (vla-get-InsertionPoint obj))
                    (setq pt (vla-get-TextAlignmentPoint obj))
                )

                ;; 获取原本的文字属性
                (setq height (vla-get-Height obj))
                (setq style (vla-get-StyleName obj))
                (setq layer (vla-get-Layer obj))
                (setq rot (vla-get-Rotation obj))

                ;; 智能映射新分式的对齐方式
                (setq mtxtAttach 7) 
                (cond
                  ((= align 1) (setq mtxtAttach 8)) 
                  ((= align 2) (setq mtxtAttach 9)) 
                  ((= align 4) (setq mtxtAttach 5)) 
                )

                ;; 原地创建新多行文字
                (setq newMtext (vla-AddMText space pt 0 newStr))

                ;; 继承属性
                (vla-put-Height newMtext height)
                (vla-put-StyleName newMtext style)
                (vla-put-Layer newMtext layer)
                (vla-put-Rotation newMtext rot)

                ;; 吸附坐标
                (vla-put-AttachmentPoint newMtext mtxtAttach)
                (vla-put-InsertionPoint newMtext pt)

                ;; 删除原单行文字
                (vla-Delete obj)
              )
              (progn
                ;; 如果已经是多行文字，直接更新
                (vla-put-TextString obj newStr)
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (vla-EndUndoMark doc)
      (princ "\n完美转换！已自动清理多余百分号，并原地生成分式。")
    )
  )
  (princ)
)