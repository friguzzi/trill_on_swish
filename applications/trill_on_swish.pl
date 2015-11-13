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

:- module(trill_on_swish_app,
	  [
	  ]).
:- use_module(library(pengines)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).
:- use_module(rdfql(sparql_csv_result)).

:- use_module(library(trill_on_swish/config)).
:- use_module(library(trill_on_swish/page), []).
:- use_module(library(trill_on_swish/storage)).
:- use_module(library(trill_on_swish/include)).
:- use_module(library(trill_on_swish/csv)).
:- use_module(library(trill_on_swish/examples)).
:- use_module(library(trill_on_swish/help)).
:- use_module(library(trill_on_swish/highlight)).
:- use_module(library(trill_on_swish/markdown)).
:- use_module(library(trill_on_swish/render)).
:- use_module(library(trill_on_swish/template_hint)).


		 /*******************************
		 *	       PATHS		*
		 *******************************/

http:location(trill_on_swish, root(trill_on_swish), []).

user:file_search_path(render, library(trill_on_swish/render)).


		 /*******************************
		 *	      CONFIG		*
		 *******************************/

:- multifile
	trill_on_swish_config:config/2,
	trill_on_swish_config:source_alias/2.

%%	trill_on_swish_config:config(?Config, ?Value) is nondet.
%
%	All solutions of this predicate are  available in the JavaScript
%	object config.trill_on_swish.Config. Config must be an  atom that is also
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

trill_on_swish_config:config(show_beware,    false).
trill_on_swish_config:config(tabled_results, false).
trill_on_swish_config:config(application,    trill_on_swish).
trill_on_swish_config:config(csv_formats,    [rdf, prolog]).
trill_on_swish_config:config(public_access,  true).

%%     trill_on_swish_config:source_alias(Alias, Options) is nondet.
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

:- multifile trill_on_swish_csv:write_answers/2.

trill_on_swish_csv:write_answers(Answers, VarTerm) :-
        Answers = [H|_],
        functor(H, rdf, _), !,
        sparql_write_csv_result(
            current_output,
            select(VarTerm, Answers),
            []).


                 /*******************************
                 *   CREATE SWISH APPLICATION   *
                 *******************************/

:- multifile
	pengines:prepare_module/3.

:- pengine_application(trill_on_swish).
:- use_rendering(trill_on_swish:rdf).
:- use_module(trill_on_swish:library(trill_on_swish/render)).
:- use_module(trill_on_swish:library(trill_on_swish/trace)).
:- use_module(trill_on_swish:library(pengines_io)).
:- use_module(trill_on_swish:library(semweb/rdf_db)).
:- use_module(trill_on_swish:library(semweb/rdfs)).
:- use_module(trill_on_swish:library(semweb/rdf_optimise)).
:- use_module(trill_on_swish:library(semweb/rdf_litindex)).
:- use_module(trill_on_swish:library(aggregate)).
pengines:prepare_module(Module, trill_on_swish, _Options) :-
	pengines_io:pengine_bind_io_to_html(Module).

% Libraries that are nice to have in SWISH, but cannot be loaded
% because they use directives that are considered unsafe.  We load
% them here, so they only need to be imported, which is just fine.

:- use_module(library(clpfd), []).
:- use_module(library(clpb), []).

% rendering libraries

:- use_module(library(trill_on_swish/render/table),    []).
:- use_module(library(trill_on_swish/render/rdf),      []).
:- use_module(library(trill_on_swish/render/graphviz), []).
:- use_module(library(trill_on_swish/render/c3),	      []).
