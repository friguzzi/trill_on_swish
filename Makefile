# Create a ClioPatria SWISH package from the SWISH distribution.

FONTDIR=web/bower_components/bootstrap/dist/fonts
DIRS=lib/trill_on_swish lib/trill_on_swish/render web/icons web/help $(FONTDIR)
SWISHLIB=trill_on_swish_storage.pl trill_on_swish_page.pl trill_on_swish_help.pl trill_on_swish_examples.pl trill_on_swish_config.pl trill_on_swish_gitty.pl \
	 trill_on_swish_highlight.pl trill_on_swish_render.pl trill_on_swish_template_hint.pl trill_on_swish_search.pl trill_on_swish_form.pl translate_rdf.pl \
	 trill/trill.pl
RENDER=table.pl
LIBS=	$(addprefix lib/trill_on_swish/, $(SWISHLIB)) \
	$(addprefix lib/trill_on_swish/render/, $(RENDER))
JS=web/js/trill_on_swish-min.js web/js/trill_on_swish-min.js.gz web/js/tos_require.js
CSS=web/css/trill_on_swish-min.css web/css/trill_on_swish-min.css.gz
ICON_FILES=owl_25_years.png dead.png error.png running.gif page-fold-20.png
ICONS=$(addprefix web/icons/, $(ICON_FILES))
HELP=$(addprefix web/help/, $(notdir $(wildcard src/web/help/*.html)))
FONTFILES=glyphicons-halflings-regular.ttf \
	  glyphicons-halflings-regular.woff
FONTS=$(addprefix $(FONTDIR)/, $(FONTFILES))

all:	$(DIRS) $(LIBS) $(JS) $(CSS) $(ICONS) $(HELP) $(FONTS)

$(DIRS):
	mkdir -p $@

lib/trill_on_swish/%: src/lib/%
	rsync -u $< $@

web/js/trill_on_swish-min.js: src/web/js/trill_on_swish-min.js
	rsync -u $< $@
web/js/trill_on_swish-min.js.gz: src/web/js/trill_on_swish-min.js.gz
	rsync -u $< $@
web/js/tos_require.js: src/web/bower_components/requirejs/require.js
	rsync -u $< $@

web/css/%: src/web/css/%
	rsync -u $< $@

web/icons/%: src/web/icons/%
	rsync -u $< $@

web/help/%: src/web/help/%
	rsync -u $< $@

$(FONTDIR)/%: src/$(FONTDIR)/%
	rsync -u $< $@

clean::
	rm -f $(LIBS) $(JS) $(CSS) $(ICONS) $(HELP)
