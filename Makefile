TARGET_DIR=$(HOME)/.config/doom/
EMACSD=$(HOME)/.config/emacs
FILES=init.el config.el packages.el site-lisp/my-org-config.el site-lisp/my-roam-config.el site-lisp/my-gptel-config.el local-org-config.sample.el secrets-sample.el
SOURCES=$(addprefix doom/,$(FILES))
TARGETS=$(addprefix $(TARGET_DIR),$(FILES))
VALE_STYLE_DIR=$(HOME)/.vale-styles/
VALE_STYLE_SOURCES=config Microsoft/Microsoft proselint/proselint write-good/write-good readability/Readability
VALE_STYLES=$(addprefix $(VALE_STYLE_DIR),$(VALE_STYLE_SOURCES))
ORG_DIR=$(HOME)/Org
CAPTURE_TEMPLATE_DIR=$(ORG_DIR)/capture-templates/
CAPTURE_TEMPLATE_SOURCES=daily_summary.org staff_meeting.org staff_meeting_as_attendee.org incident.org todo.org draft.org personal_draft.org interview.org bookmark.org meeting.org coe.org
CAPTURE_TEMPLATES=$(addprefix $(CAPTURE_TEMPLATE_DIR), $(CAPTURE_TEMPLATE_SOURCES))

all: sync vale daemon

${VALE_STYLE_DIR}%: Vale/Styles/%
	mkdir -p $(dir $@)
	cp -r $< $(dir $@)

${CAPTURE_TEMPLATE_DIR}%: doom/site-lisp/capture-templates/%
	mkdir -p ${CAPTURE_TEMPLATE_DIR}
	cp $< $@

${TARGET_DIR}%.el: doom/%.el
	mkdir -p $(dir $@)
	cp $< $@

touch:
	touch $(SOURCES) $(CAPTURE_TEMPLATE_SOURCES)

files: $(TARGETS) $(CAPTURE_TEMPLATES) $(TARGET_DIR)/local-org-config.el

$(TARGET_DIR)/local-org-config.el:
	if [ -e $(TARGET_DIR)/local-org-config.el ]; then echo "foo"; else cp doom/local-org-config.sample.el $(TARGET_DIR)/local-org-config.el; fi

sync: files
	$(EMACSD)/bin/doom sync

$(HOME)/.vale.ini: Vale/vale.ini
	cp Vale/vale.ini $(HOME)/.vale.ini

vale: install-vale $(HOME)/.vale.ini $(VALE_STYLES)

install-vale:
	@if ! command -v vale >/dev/null 2>&1; then \
		echo "Vale not found. Installing via Homebrew..."; \
		if command -v brew >/dev/null 2>&1; then \
			brew install vale; \
		else \
			echo "Error: Homebrew not found. Please install Vale manually:"; \
			echo "  - Via Homebrew: brew install vale"; \
			echo "  - Via Go: go install github.com/errata-ai/vale/v2/cmd/vale@latest"; \
			echo "  - Download from: https://github.com/errata-ai/vale/releases"; \
			exit 1; \
		fi; \
	else \
		echo "Vale already installed: $$(vale --version)"; \
	fi

vale-clean:
	rm -rf $(VALE_STYLE_DIR)
	rm $(HOME)/.vale.ini

run:
	emacs

.PHONY: all daemon restart touch files sync vale vale-clean run install-vale
