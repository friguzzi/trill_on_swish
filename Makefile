# Create a ClioPatria SWISH package from the SWISH distribution.

FONTDIR=web/bower_components/bootstrap/dist/fonts
DIRS=lib/trill_on_swish lib/trill_on_swish/render web/icons web/help client $(FONTDIR) \
     web/bower_components/codemirror/mode/htmlmixed \
     web/bower_components/codemirror/mode/css \
     web/bower_components/codemirror/mode/javascript
SWISHLIB=storage.pl page.pl help.pl examples.pl config.pl gitty.pl \
	 highlight.pl render.pl template_hint.pl search.pl form.pl \
	 include.pl swish_csv.pl logging.pl trace.pl markdown.pl \
	 gitty_driver_files.pl gitty_driver_bdb.pl gitty_tools.pl \
	 swish_debug.pl profiles.pl procps.pl
RENDER=table.pl graphviz.pl c3.pl
LIBS=	$(addprefix lib/trill_on_swish/, $(SWISHLIB)) \
	$(addprefix lib/trill_on_swish/render/, $(RENDER))
JS=web/js/swish-min.js web/js/swish-min.js.gz web/js/require.js
CSS=web/css/swish-min.css web/css/swish-min.css.gz web/css/print.css
ICON_FILES=dead.png error.png running.gif page-fold-20.png \
	   red_bird.svg red_bird_op.svg pl.png swinb.png owl.png \
	   select.png  wip.png rb_favicon.ico trill-touch-icon.png
ICONS=$(addprefix web/icons/, $(ICON_FILES))
HELP=$(addprefix web/help/, $(notdir $(wildcard src/web/help/*.html)))
FONTFILES=glyphicons-halflings-regular.ttf \
	  glyphicons-halflings-regular.eot \
	  glyphicons-halflings-regular.woff \
	  glyphicons-halflings-regular.woff2
FONTS=$(addprefix $(FONTDIR)/, $(FONTFILES))
CLIENTFILES=swish-ask.sh README.md sin-table.html
CLIENTS=$(addprefix client/, $(CLIENTFILES))
EXAMPLESFILES=biopaxLevel3.pl BRCA.pl DBPedia.pl examples.swinb \
              peoplePets.pl vicodi.pl commander.pl johnEmployee.pl index.json
EXAMPLES=$(addprefix examples/, $(EXAMPLESFILES))
PROFILESFILES=00-Empty.pl 00-Empty.swinb 10-Non-Probabilistic.pl \
             10-Non-Probabilistic.swinb 20-Probabilistic.pl 20-Probabilistic.swinb
PROFILES=$(addprefix profiles/, $(PROFILESFILES))
CMFILES=mode/htmlmixed/htmlmixed.js \
	mode/javascript/javascript.js \
	mode/css/css.js
CM=$(addprefix web/bower_components/codemirror/, $(CMFILES))

all:	$(DIRS) $(LIBS) $(JS) $(CSS) $(ICONS) $(HELP) $(FONTS) $(CLIENTS) \
	$(CM) $(EXAMPLES) $(PROFILES)

$(DIRS):
	mkdir -p $@

lib/trill_on_swish/%: src/lib/%
	rsync -u $< $@
client/%: src/client/%
	rsync -u $< $@
client/swish-ask.sh: src/client/swish-ask.sh
	sed -e 's/:3050}/:3020}/' -e 's/-prolog}/-rdf}/' $< > $@
	chmod +x $@

web/js/swish-min.js: src/web/js/swish-min.js
	rsync -u $< $@
web/js/swish-min.js.gz: src/web/js/swish-min.js.gz
	rsync -u $< $@
web/js/require.js: src/web/bower_components/requirejs/require.js
	rsync -u $< $@

web/css/%: src/web/css/%
	rsync -u $< $@

web/icons/%: src/web/icons/%
	rsync -u $< $@

web/help/%: src/web/help/%
	rsync -u $< $@

examples/%: src/examples/%
	rsync -u $< $@

profiles/%: src/profiles/%
	rsync -u $< $@

$(FONTDIR)/%: src/$(FONTDIR)/%
	rsync -u $< $@

web/bower_components/codemirror/%: src/web/bower_components/codemirror/%
	rsync -u $< $@

clean::
	rm -f $(LIBS) $(JS) $(CSS) $(ICONS) $(HELP)
