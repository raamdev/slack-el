;;; test-helper --- Test helper for slack-el

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)
(require 'undercover)
(undercover "*.el" "slack-el/*.el"
            (:exclude "*-test.el")
            (:send-report nil)
            (:report-file "/tmp/undercover-report.json"))

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar slack-el-test-path
  (f-dirname (f-this-file)))

(defvar slack-el-root-path
  (f-parent slack-el-test-path))

(defvar slack-el-sandbox-path
  (f-expand "sandbox" slack-el-test-path))

(when (f-exists? slack-el-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" slack-el-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory slack-el-sandbox-path))
     (when (f-exists? slack-el-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir slack-el-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'slack-el)

(provide 'test-helper)
;;; test-helper.el ends here
