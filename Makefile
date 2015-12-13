# Create a ClioPatria SWISH package from the SWISH distribution.

FONTDIR=web/bower_components/bootstrap/dist/fonts
DIRS=lib/trill_on_swish lib/trill_on_swish/render web/icons web/help client $(FONTDIR)
SWISHLIB=storage.pl page.pl help.pl examples.pl config.pl gitty.pl \
	 highlight.pl render.pl template_hint.pl search.pl form.pl \
	 include.pl csv.pl logging.pl trace.pl markdown.pl \
	 translate_rdf.pl trill/trill.pl
RENDER=table.pl graphviz.pl c3.pl
LIBS=	$(addprefix lib/trill_on_swish/, $(SWISHLIB)) \
	$(addprefix lib/trill_on_swish/render/, $(RENDER))
JS=web/js/trill_on_swish-min.js web/js/trill_on_swish-min.js.gz web/js/require.js
CSS=web/css/trill_on_swish-min.css web/css/trill_on_swish-min.css.gz web/css/print.css
ICON_FILES=dead.png error.png running.gif page-fold-20.png \
	   red_bird.svg pl.png swinb.png owl.png \
	   select.png
ICONS=$(addprefix web/icons/, $(ICON_FILES))
HELP=$(addprefix web/help/, $(notdir $(wildcard src/web/help/*.html)))
FONTFILES=glyphicons-halflings-regular.ttf \
	  glyphicons-halflings-regular.eot \
	  glyphicons-halflings-regular.woff \
	  glyphicons-halflings-regular.woff2
FONTS=$(addprefix $(FONTDIR)/, $(FONTFILES))
CLIENTFILES=trill_on_swish-ask.sh README.md sin-table.html
CLIENTS=$(addprefix client/, $(CLIENTFILES))

all:	$(DIRS) $(LIBS) $(JS) $(CSS) $(ICONS) $(HELP) $(FONTS) $(CLIENTS)

$(DIRS):
	mkdir -p $@

lib/trill_on_swish/%: src/lib/%
	rsync -u $< $@
client/%: src/client/%
	rsync -u $< $@
client/trill_on_swish-ask.sh: src/client/trill_on_swish-ask.sh
	sed -e 's/:3050}/:3020}/' -e 's/-prolog}/-rdf}/' $< > $@
	chmod +x $@

web/js/trill_on_swish-min.js: src/web/js/trill_on_swish-min.js
	rsync -u $< $@
web/js/trill_on_swish-min.js.gz: src/web/js/trill_on_swish-min.js.gz
	rsync -u $< $@
web/js/require.js: src/web/bower_components/requirejs/require.js
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
