;; Required testing libraries
(require 'cl)
(require 'el-mock)

;; Load the library 
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'phing)

;; Setup test paths
(setq test-directory (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(setq fixture-directory (concat test-directory "/fixtures/"))

;; Helper functions

(defun read-fixture (file)
  "Reads FILE from the fixtures directory and returns it as parsed JSON."
  
  ;; Get the file path
  (let* ((file-path (concat test-directory "/fixtures/" file))
         (file-contents (with-temp-buffer
                          (insert-file-contents file-path)
                          (buffer-string))))
    (json-read-from-string file-contents)))
