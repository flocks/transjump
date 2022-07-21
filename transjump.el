;;;  -*- lexical-binding: t -*-

;;; Code:
(require 'transient)

(defcustom transjump-folders nil
  "List of all favorites folders to the transient menu.
The shape is a plist (:name \"name\" :key 'k' :path \"~/path/to/folder\") "
  :type '(plist))

(defvar transjump--action 'file
  "The action to perform when we activate the prefix.
either we open the file, or we create/swith to a named tab")

(transient-define-prefix transjump (arg)
  "Transient menu to quickly access favorites folders"
  [:description "Folders"
				:setup-children transjump-setup-folders
				:pad-keys t]
  (interactive "P")
  (setq transjump--action (if arg 'tab 'file))
  (transient-setup 'transjump))

(defun transjump-setup-folders (_)
  "Generate all suffixes for transjump prefix"
  (cl-mapcan
   (lambda (folder)
	 (transient--parse-child
	  'transjump
	  (list (plist-get folder :key)
			(plist-get folder :name)
			(lambda ()
			  (interactive)
			  (if (eq transjump--action 'file)
				  (find-file (plist-get folder :path))
				(transjump--switch-to-tab (plist-get folder :name)))))))
   transjump-folders))

(defun transjump--switch-to-tab (tab)
  "Switch to TAB. Create it if it doesn't exist"
  (if (transjump--tab-exist-p tab)
	  (tab-bar-switch-to-tab tab)
	(when-let ((folder (transjump--find-folder-by-name tab)))
	  (tab-bar-new-tab)
	  (tab-bar-rename-tab tab)
	  (find-file (plist-get folder :path)))))

(defun transjump--find-folder-by-name (name)
  (catch 'folder
	(dolist (folder transjump-folders)
	  (when (string= (plist-get folder :name) name)
		(throw 'folder folder)))))

(defun transjump--tab-exist-p (tab)
  "Whether TAB exist or not."
  (member tab (transjump--get-tabs)))

(defun transjump--get-tabs ()
  "Get the names of all existing tabs"
  (let ((tabs (funcall tab-bar-tabs-function)))
    (mapcar
     (lambda (tab)
       (cdr (assoc 'name (cdr tab))))
     tabs)))


(provide 'transjump)

