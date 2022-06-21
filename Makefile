TARGET_DIR=$(HOME)/.doom.d/
FILES=init.el config.el packages.el site-lisp/my-org-config.el site-lisp/my-roam-config.el
SOURCES=$(addprefix doom.d/,$(FILES))
TARGETS=$(addprefix $(TARGET_DIR),$(FILES))
VALE_STYLE_DIR=$(HOME)/.vale-styles/
VALE_STYLE_SOURCES=Google proselint Microsoft alex readability write-good
VALE_STYLES=$(addprefix $(VALE_STYLE_DIR),$(VALE_STYLE_SOURCES))

${VALE_STYLE_DIR}%: Vale/%
	mkdir -p $(VALE_STYLE_DIR)
	cp -r $</$(notdir $<) $@

${TARGET_DIR}%.el: doom.d/%.el
	mkdir -p $(dir $@)
	cp $< $@

all: sync vale

touch:
	touch $(SOURCES)

files: $(TARGETS)

sync: files
	$(HOME)/.emacs.d/bin/doom sync

$(HOME)/.vale.ini: Vale/vale.ini
	cp Vale/vale.ini $(HOME)/.vale.ini

$(VALE_STYLE_DIR)Vocab: Vale/vale-boilerplate/styles/Vocab
	mkdir -p $(VALE_STYLE_DIR)
	cp -r $< $@
	
vale: $(HOME)/.vale.ini $(VALE_STYLE_DIR)Vocab $(VALE_STYLES)

vale-clean:
	rm -rf $(VALE_STYLE_DIR)
	rm $(HOME)/.vale.ini
