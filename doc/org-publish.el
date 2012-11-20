(setq org-publish-project-alist
      '(("orgfiles"
	 :base-directory "~/my-software-add-ons/my-lisp/gnuplot-interface/doc/"
	 :base-extension "org"
	 :recursive t
	 :publishing-directory "~/my-software-add-ons/my-lisp/gnuplot-interface/documentation"
	 :publishing-function org-publish-org-to-html
	 :headline-levels 3
	 :section-numbers nil
	 :table-of-contents nil
	 :auto-preamble t
	 :auto-postamble nil)))