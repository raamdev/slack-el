(require 'f)
(require 'undercover)
(undercover "*.el" "slack/*.el"
            (:exclude "*-test.el")
            (:report-file "/tmp/undercover-report.json"))

(defvar slack-el-support-path
  (f-dirname load-file-name))

(defvar slack-el-features-path
  (f-parent slack-el-support-path))

(defvar slack-el-root-path
  (f-parent slack-el-features-path))

(add-to-list 'load-path slack-el-root-path)

(require 'slack)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
