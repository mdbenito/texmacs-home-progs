(with b (current-buffer)
  (if (url-scratch? b)
      (begin
        (set-style-list (append (get-style-list) (list "Pilstyle")))
        (buffer-pretend-saved b))))