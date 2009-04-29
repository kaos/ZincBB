-module(znbb_page_signup).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% Account signup
%
%%%
%   Copyright 2009 Ceti Forge
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%
%%%

-export([content/0, event/1, main/0, sidebar/0, title/0]).

-include("./lib/nitrogen/include/wf.inc").

main() -> #template{file = "dual.html"}.

title() -> "Zinc BB".

content() ->
    LSpan = "span-4",
    FSpan = "span-10",
    Items = [{"Username:", #textbox{id = username_field, next = password_field}},
	     {"Password:", #password{id = password_field, next = confirm_field}},
	     {"Confirm:", #password{id = confirm_field, next = email_field}},
	     {"Email:", #textbox{id = email_field, next = create_button}},
	     {" ",
	      #button{text = "Create Account", class = "prepend-top", id = create_button,
		      postback = create}}],
    wf:wire(create_button, username_field,
	    #validate{validators =
			  [#is_required{text = "Please, don't be shy. This is a forum!"}]}),
    wf:wire(create_button, password_field,
	    #validate{validators =
			  [#is_required{text =
					    "A password is required, unless of course you know the "
					    "secret handshake."},
			   #min_length{length = 6,
				       text =
					   "Password must be at least 6 characters, otherwise even "
					   "a script-kiddie can brute-force it."}]}),
    wf:wire(create_button, confirm_field,
	    #validate{validators =
			  [#confirm_password{password = password_field,
					     text =
						 "Passwords must match, not really a challenge is it?"}]}),
    wf:wire(create_button, email_field,
	    #validate{validators =
			  [#is_required{text =
					    "We thought everyone had an email address, but apparently "
					    "not you!"},
			   #is_email{text = "You call that an email address!?!?"}]}),
    [#h1{text = "Sign up here"},
     #panel{class = "zn_form", body = [form_builder(Items, LSpan, FSpan)]}].

sidebar() ->
    [#h3{text = "Username"},
     "Pick the name you want everyone here to know you as, "
     "yes you can even lie.  Note, valid characters are: ...",
     #h3{text = "Password", class = "prepend-top"},
     "Pick a good one, and keep it a secret.",
     #h3{text = "Email", class = "prepend-top"},
     "Your email address is used as a gravatar. If you don't "
     "have a gravatar yet, ",
     #link{url = "http://gravatar.com", text = "get one"},
     ". If you're the forgetful type, we can use your email "
     "to send you your password."].

form_builder(Items, LabelSpan, FieldSpan) ->
    [[#panel{class = LabelSpan, body = #label{text = Lbl}},
      #panel{class = FieldSpan, body = Field}, #br{}, #br{}]
     || {Lbl, Field} <- Items].

event(create) ->
    [Username] = wf:q(username_field),
    [Password] = wf:q(password_field),
    [Email] = wf:q(email_field),
    case znbb_account:create(znbb_utils:safe_bin(Username),
			     znbb_utils:safe_bin(Password), znbb_utils:safe_bin(Email))
	of
      {ok, _Account} -> wf:redirect("/");
      {error, Reason} -> errorbar:now(Reason)
    end.
