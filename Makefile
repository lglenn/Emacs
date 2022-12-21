TARGET_DIR=$(HOME)/.doom.d/
FILES=init.el config.el packages.el site-lisp/my-org-config.el site-lisp/my-roam-config.el
SOURCES=$(addprefix doom.d/,$(FILES))
TARGETS=$(addprefix $(TARGET_DIR),$(FILES))
VALE_STYLE_DIR=$(HOME)/.vale-styles/
VALE_STYLE_SOURCES=proselint Microsoft readability write-good
VALE_STYLES=$(addprefix $(VALE_STYLE_DIR),$(VALE_STYLE_SOURCES))
ORG_DIR=$(HOME)/Org
CAPTURE_TEMPLATE_DIR=$(ORG_DIR)/capture-templates/
CAPTURE_TEMPLATE_SOURCES=daily_summary.org staff_meeting_as_attendee.org incident.org todo.org
CAPTURE_TEMPLATES=$(addprefix $(CAPTURE_TEMPLATE_DIR), $(CAPTURE_TEMPLATE_SOURCES))

${VALE_STYLE_DIR}%: Vale/Styles/%
	mkdir -p $(VALE_STYLE_DIR)
	cp -r $</$(notdir $<) $@

${CAPTURE_TEMPLATE_DIR}%: doom.d/site-lisp/capture-templates/%
	mkdir -p ${CAPTURE_TEMPLATE_DIR}
	cp $< $@

${TARGET_DIR}%.el: doom.d/%.el
	mkdir -p $(dir $@)
	cp $< $@

all: sync vale

touch:
	touch $(SOURCES) $(CAPTURE_TEMPLATE_SOURCES)

files: $(TARGETS) $(CAPTURE_TEMPLATES)

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
