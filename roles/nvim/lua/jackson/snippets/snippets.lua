local ls = require("luasnip")
local s = ls.s
local fmt = require("luasnip.extras.fmt").fmt
local i = ls.insert_node
local t = ls.text_node
local c = ls.choice_node
local f = ls.function_node

-- HELPERS

--- Get the current buffer's name without its extensions.
--- @example file.test.ts -> file
--- @return string
local function get_buf_name_without_extensions()
  local filename = vim.fn.expand("%:t")

  -- return buf name now if no extensions
  if not string.find(filename, ".") then
    return filename
  end

  return vim.split(filename, ".", true)[1]
end

-- TYPESCRIPT SNIPS --

local typescript_log_snippet = s("l", fmt("console.log({});", { i(1) }))
local typescript_import = s("i", fmt("import {} from '{}';", { i(1), i(2) }))
local typescript_to_have_been_called_times = s("ct", fmt("expect({}).toHaveBeenCalledTimes({});", { i(1), i(2) }))
local typescript_to_have_been_called_with = s("cw", fmt("expect({}).toHaveBeenCalledWith({});", { i(1), i(2) }))
local typescript_expect = s("e", fmt("expect({})", { i(1) }))
local typescript_mock_res = s(
  "res",
  fmt("{}.fn().mockResolvedValueOnce({});", {
    c(1, { t("vi"), t("jest") }),
    i(2),
  })
)
local typescript_mock_rej = s(
  "rej",
  fmt("{}.fn().mockRejectedValueOnce({});", {
    c(1, { t("vi"), t("jest") }),
    i(2),
  })
)

local typescriptreact_use_ref = s(
  "ur",
  fmt(
    [[
const {}Ref = useRef{}({});
  ]],
    {
      i(1),
      i(2),
      i(3),
    }
  )
)

local typescriptreact_use_state = s(
  "us",
  fmt(
    [[
const [{}, set{}] = useState{}({});
]],
    {
      i(1),
      f(function(state_name)
        return (state_name[1][1]:gsub("^%l", string.upper))
      end, { 1 }),
      i(2),
      i(3),
    }
  )
)

local typescriptreact_use_effect = s(
  "ue",
  fmt(
    [[
useEffect(() => {{
  {}
}}{});
]],
    { i(1), i(2) }
  )
)

local typescript_describe = s(
  "de",
  fmt(
    [[
describe('{} unit tests', () => {{
  {}
}});
]],
    {
      c(1, { f(get_buf_name_without_extensions), i(nil, "") }),
      i(2),
    }
  )
)

local typescript_it = s(
  "it",
  fmt(
    [[
it('should {}', async () => {{
  {}
}});
]],
    { i(1), i(2) }
  )
)

local typescript_before_each = s(
  "be",
  fmt(
    [[
beforeEach(() => {{
  {}
}});
]],
    { i(1) }
  )
)

local typescript_const = s("c", fmt("{}const {} = {};", { c(1, { t(""), t("export ") }), i(2), i(3) }))

ls.add_snippets("typescript", {
  typescript_log_snippet,
  typescript_to_have_been_called_times,
  typescript_expect,
  typescript_import,
  typescript_it,
  typescript_describe,
  typescript_before_each,
  typescript_const,
  typescript_to_have_been_called_with,
  typescript_mock_res,
  typescript_mock_rej,
})

ls.add_snippets("typescriptreact", {
  typescriptreact_use_effect,
  typescriptreact_use_state,
  typescriptreact_use_ref,
})

ls.filetype_extend("typescriptreact", { "typescript" })

-- LUA SNIPS

local lua_use = s(
  "u",
  fmt(
    [[
use ({{
  "{}",
  config = function()
    {}
  end,
}})
]],
    {
      i(1),
      i(2),
    }
  )
)

ls.add_snippets("lua", {
  lua_use,
})
