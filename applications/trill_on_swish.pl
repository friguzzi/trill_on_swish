/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(swish_app,
	  [
	  ]).
:- use_module(library(pengines)).
:- use_module(library(option)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).
:- use_module(rdfql(sparql_csv_result)).

:- use_module(library(trill_on_swish/config)).
:- use_module(library(trill_on_swish/page), []).
:- use_module(library(trill_on_swish/storage)).
:- use_module(library(trill_on_swish/include)).
:- use_module(library(trill_on_swish/swish_csv)).
:- use_module(library(trill_on_swish/examples)).
:- use_module(library(trill_on_swish/profiles)).
:- use_module(library(trill_on_swish/help)).
:- use_module(library(trill_on_swish/highlight)).
:- use_module(library(trill_on_swish/markdown)).
:- use_module(library(trill_on_swish/render)).
:- use_module(library(trill_on_swish/template_hint)).


		 /*******************************
		 *	       PATHS		*
		 *******************************/

http:location(swish, root(trill_on_swish), []).

user:file_search_path(swish, cp_application(cpack/trill_on_swish)).
user:file_search_path(render, library(trill_on_swish/render)).


		 /*******************************
		 *	      CONFIG		*
		 *******************************/

:- multifile
	swish_config:config/2,
	swish_config:source_alias/2.

%%	swish_config:config(?Config, ?Value) is nondet.
%
%	All solutions of this predicate are  available in the JavaScript
%	object config.swish.Config. Config must be an  atom that is also
%	a valid JavaScript identifier. Value  must   be  a value that is
%	valid for json_write_dict/2. Most configurations  are also saved
%	in the application preferences. These   are  marked [P]. Defined
%	config parameters:
%
%	  - show_beware
%	  [P] If `true`, show the *Beware* modal dialog on startup
%	  - tabled_results
%	  [P] If `true`, check the _table results_ checkbox by default.
%	  - application
%	  Name of the Pengine application.
%	  - csv_formats
%	  [P] CSV output formats offered. For example, ClioPatria
%	  defines this as [rdf,prolog]. The first element is default.
%        - public_access
%        If lib/authenticate.pl is loaded and this flag is `true`,
%        _all_ access to SWISH demands authentication.  If false,
%        only running queries and saving files is restricted. Note
%        that this flag has no effect if no authentication module is
%        loaded.
%        - ping
%        Ping pengine status every N seconds.  Updates sparkline
%        chart with stack usage.
%         - nb_eval_script
%         Evaluate scripts in HTML cells of notebooks?

swish_config:config(show_beware,    false).
swish_config:config(tabled_results, false).
swish_config:config(application,    swish).
swish_config:config(csv_formats,    [rdf, prolog]).
swish_config:config(public_access,  true).
swish_config:config(ping,           10).
swish_config:config(notebook,       _{eval_script: true}).

%%     swish_config:source_alias(Alias, Options) is nondet.
%
%      Specify access for files below a given _alias_. Options define
%
%        - access(Access)
%        One of `read` or `both`.  Default is `read`.
%        - if(Condition)
%        Provide additional conditions.  Defined conditions are:
%          - loaded
%          Only provide access to the file if it is loaded.


		 /*******************************
		 *	        CSV		*
		 *******************************/

:- multifile
	swish_csv:write_answers/2,
	swish_csv:write_answers/3.

swish_csv:write_answers(Answers, VarTerm) :-
        Answers = [H|_],
        functor(H, rdf, _), !,
        sparql_write_csv_result(
            current_output,
            select(VarTerm, Answers),
            []).

swish_csv:write_answers(Answers, VarTerm, Options) :-
        Answers = [H|_],
        functor(H, rdf, _),
	option(page(1), Options), !,
        sparql_write_csv_result(
            current_output,
            select(VarTerm, Answers),
            [ bnode_state(_-BNodes)
	    ]),
	nb_setval(rdf_csv_bnodes, BNodes).
swish_csv:write_answers(Answers, VarTerm, Options) :-
        Answers = [H|_],
        functor(H, rdf, _),
	option(page(Page), Options),
	Page > 1, !,
	nb_getval(rdf_csv_bnodes, BNodes0),
        sparql_write_csv_result(
            current_output,
            select(VarTerm, Answers),
            [ http_header(false),
	      header_row(false),
	      bnode_state(BNodes0-BNodes)
	    ]),
	nb_setval(rdf_csv_bnodes, BNodes).
swish_csv:write_answers(Answers, VarTerm, _Options) :-
	swish_csv:write_answers(Answers, VarTerm).


                 /*******************************
                 *   CREATE SWISH APPLICATION   *
                 *******************************/

:- multifile
	pengines:prepare_module/3.

:- pengine_application(swish).
:- use_rendering(swish:rdf).
:- use_module(swish:library(trill_on_swish/render)).
:- use_module(swish:library(trill_on_swish/trace)).
:- use_module(swish:library(pengines_io)).
:- use_module(swish:library(semweb/rdf_db)).
:- use_module(swish:library(semweb/rdfs)).
:- use_module(swish:library(semweb/rdf_optimise)).
:- use_module(swish:library(semweb/rdf_litindex)).
:- use_module(swish:library(solution_sequences)).
:- use_module(swish:library(aggregate)).
pengines:prepare_module(Module, swish, _Options) :-
	pengines_io:pengine_bind_io_to_html(Module).

% Libraries that are nice to have in SWISH, but cannot be loaded
% because they use directives that are considered unsafe.  We load
% them here, so they only need to be imported, which is just fine.

:- use_module(library(clpfd), []).
:- use_module(library(clpb), []).
:- if(exists_source(library(semweb/rdf11))).
:- use_module(library(semweb/rdf11), []).
:- endif.

% rendering libraries

:- use_module(library(trill_on_swish/render/table),    []).
:- use_module(library(trill_on_swish/render/rdf),      []).
:- use_module(library(trill_on_swish/render/graphviz), []).
:- use_module(library(trill_on_swish/render/c3),	      []).

:- use_module(library(trill)).
