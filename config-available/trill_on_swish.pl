:- module(conf_trill_on_swish, []).
:- use_module(cliopatria(hooks)).

/** <module> Add Prolog interaction to ClioPatria
*/

:- multifile user:file_search_path/2.

% tell SWISH where to find its parts.   The last clause allows adding an
% =examples=  directory  in  the  main   directory  holding  application
% specific examples.

user:file_search_path(trill_on_swish_web, web(.)).
user:file_search_path(tos_example,   cpacks(trill_on_swish/examples)).
user:file_search_path(tos_example,	 examples).

% Load swish.  You need this.
:- use_module(applications(trill_on_swish)).
% Load the authentication hook. When loaded, ClioPatria users with admin
% rights can use SWISH without sandboxing security
:- use_module(library(trill_on_swish/trill_on_swish_cp_authenticate)).

%%      cliopatria:menu_item(-Item, -Label) is nondet.
%
%       Add SWISH to the Query menu.

cliopatria:menu_item(400=query/trill_on_swish, 'TRILL on SWISH shell').
