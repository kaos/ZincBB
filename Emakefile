%  Project EMakefile
%
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

% === Make ZincBB App ===================================

{ "./lib/zincBB/src/*", [
	{i, "./lib/zincBB/include"},
	{i, "./lib/nitrogen/include"},
	{outdir, "./lib/zincBB/ebin" },
	debug_info
    ]}.
{ "./lib/zincBB/src/*/*", [
	{i, "./lib/zincBB/include"},
	{i, "./lib/nitrogen/include"},
	{outdir, "./lib/zincBB/ebin" },
	debug_info
    ]}.


% === Mochiweb ==========================================

{ "./lib/mochiweb/src/*", [
	{outdir, "./lib/mochiweb/ebin" },
	debug_info
    ]}.


% === Nitrogen ==========================================

{ "./lib/nitrogen/src/*", [
	{i, "./lib/nitrogen/include"},
	{outdir, "./lib/nitrogen/ebin" },
	debug_info
    ]}.
{ "./lib/nitrogen/src/*/*", [
	{i, "./lib/nitrogen/include"},
	{outdir, "./lib/nitrogen/ebin" },
	debug_info
    ]}.
{ "./lib/nitrogen/src/*/*/*", [
	{i, "./lib/nitrogen/include"},
	{outdir, "./lib/nitrogen/ebin" },
	debug_info
    ]}.
