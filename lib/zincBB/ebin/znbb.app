{application, znbb, [
	{description,  "ZincBB Forum"},
	{vsn, "0.01a"},
	{env, [
		% =============================================
		% = ZincBB Configuration Parameters
		% =============================================

		% You may wish to change the following
		{ip, {0,0,0,0}},
		{port, 8001},
		{session_timeout, 20},

		% Nitrogen specific, should be left alone.
		{sign_key, randomized},
		{platform, mochiweb},
		{wwwroot, "./wwwroot"},
		{templateroot, "./templates"},
		{hooks_module, znbb_nitrogen}
	    ]},
	{mod, {znbb_app, []}},
	{modules, [
		znbb_app,
		znbb_sup,
		znbb_nitrogen,
		znbb_account,
		znbb_library,
		znbb_thread,
		znbb_utils,
		znbb_vault,
		znbb_mnesia_db,

		znbb_page_index,
		znbb_page_thread,
		znbb_page_signup,
		znbb_page_profile
	    ]},
	{applications, [kernel, stdlib, crypto]}
    ]}.
