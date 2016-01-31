EMACS_COMMAND   ?= emacs
EVAL := $(EMACS_COMMAND) --no-site-file --batch -l ~/Catalysis-Preprint-Archive/melpa/package-build.el --eval

index:
	$(EMACS_COMMAND) --batch \
		-l ~/Catalysis-Preprint-Archive/melpa/package-build.el \
		-l /Users/jkitchin/Dropbox/kitchingroup/jmax/init.el \
		--eval '(setq org-export-babel-evaluate t)' \
		--eval '(progn (find-file "index.org") (org-html-export-to-html))'


deploy:
	git add *
	git commit -am "deploying"
	git push
