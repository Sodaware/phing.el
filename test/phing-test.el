(ert-deftest phing-test/can-get-targets-for-build-file ()
  (let* ((file (concat fixture-directory "simple-build.xml"))
         (targets (phing-get-target-nodes file)))
    (should (= 3 (length targets)))))
