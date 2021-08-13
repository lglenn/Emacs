TARGET_DIR=$(HOME)/.doom.d/
FILES=init.el config.el packages.el site-lisp/my-org-config.el site-lisp/my-roam-config.el
SOURCES=$(addprefix doom.d/,$(FILES))
TARGETS=$(addprefix ${TARGET_DIR},$(FILES))

${TARGET_DIR}%.el: doom.d/%.el
	mkdir -p $(dir $@)
	cp $< $@

all: sync

touch:
	touch $(SOURCES)

files: $(TARGETS)

sync: files
	~/.emacs.d/bin/doom sync
