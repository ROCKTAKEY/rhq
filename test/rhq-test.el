;;; rhq-test.el --- test for rhq

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords:

;; Version: 0.0.0
;; Package-Requires:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'rhq)

(ert-deftest rhq--check-executable-availability ()
  (rhq--check-executable-availability))

(ert-deftest rhq--dirname-url-cons-with-protocol ()
  (should (equal (rhq--make-dirname-url-cons "https://example.com/user/repo" "/home/user/rhq/" "protocol")
                 (cons "/home/user/rhq/example.com/user/repo" "https://example.com/user/repo"))))

(ert-deftest rhq--dirname-url-cons-absolute-path ()
  (should (equal (rhq--make-dirname-url-cons "/path/to/repo" "/home/user/rhq/" "protocol")
                 (cons "/path/to/repo" nil))))

(ert-deftest rhq--dirname-url-cons-relative-path ()
  (should (equal (rhq--make-dirname-url-cons "example.com/user/repo" "/home/user/rhq/" "protocol")
                 (cons "/home/user/rhq/example.com/user/repo" "protocol://example.com/user/repo"))))

(provide 'rhq-test)
;;; rhq-test.el ends here
