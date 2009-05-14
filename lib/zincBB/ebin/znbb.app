{application, znbb, [
	{description,  "ZincBB Forum"},
	{vsn, "0.02a"},
	{mod, {znbb_app, []}},
	{modules, [
		znbb_app,
		znbb_sup,
		znbb_nitrogen,
		znbb_distid,
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
	{env, [
		% Nitrogen specific, should be left alone.
		{sign_key, randomized},
		{platform, mochiweb},
		{wwwroot, "./wwwroot"},
		{templateroot, "./templates"},
		{hooks_module, znbb_nitrogen}
	    ]},
	{applications, [kernel, stdlib, crypto]}
    ]}.
