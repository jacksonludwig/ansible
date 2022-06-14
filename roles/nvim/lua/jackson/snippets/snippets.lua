local ls = require("luasnip")

-- Snippet creator
-- s(<trigger>, <nodes>)
local s = ls.s

-- Format node.
-- It takes a format string, and a list of nodes
-- fmt(<fmt_string>, { ...nodes })
local fmt = require("luasnip.extras.fmt").fmt

-- This is an insert node
-- It takes a position (like $1) and optionally some default text
-- i(<position>, [default_text])
local i = ls.insert_node

-- Repeats a node
-- rep(<position>)
local rep = require("luasnip.extras").rep

-- TYPESCRIPT SNIPS --

local typescript_log_snippet = s("log", fmt("console.log({})", { i(1) }))
local typescript_import = s("i", fmt("import {} from '{}';", { i(1), i(2) }))
local typescript_to_have_been_called_times = s("thbc", fmt("toHaveBeenCalledTimes({})", { i(1) }))
local typescript_expect = s("e", fmt("expect({})", { i(1) }))
local typescript_describe = s("de", fmt([[
describe('{} unit tests', () => {{
  {}
}});
]], { i(1), i(2) }))
local typescript_it = s("it", fmt([[
it('should {}', async () => {{
  {}
}});
]], { i(1), i(2) }))
local typescript_before_each = s("be", fmt([[
beforeEach(() => {{
  {}
}});
]], { i(1) }))
local typescript_const = s("c", fmt("const {} = {};", { i(1), i(2) }))
local typescript_export_const = s("ec", fmt("export const {} = {};", { i(1), i(2) }))

ls.add_snippets("typescript", {
  typescript_log_snippet,
  typescript_to_have_been_called_times,
  typescript_expect,
  typescript_import,
  typescript_it,
  typescript_describe,
  typescript_before_each,
  typescript_const,
  typescript_export_const,
})

ls.filetype_extend("typescriptreact", { "typescript" })
