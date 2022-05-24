-- packer bootstrap
local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
local packer_bootstrap = nil
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  })
end

-- packer
require("packer").startup(function(use)
  use({
    "wbthomason/packer.nvim",
  })

  use({
    "tpope/vim-repeat",
    "tpope/vim-abolish",
    "tpope/vim-surround",
  })

  use({
    "ggandor/lightspeed.nvim",
    config = function()
      vim.keymap.set("n", "s", "<Plug>Lightspeed_omni_s")
    end,
  })

  use({
    "dkarter/bullets.vim",
    config = function()
      vim.g.bullets_enabled_file_types = { "markdown", "text", "gitcommit", "scratch" }
    end,
  })

  use({
    "jghauser/kitty-runner.nvim",
    config = function()
      require("kitty-runner").setup({
        use_keymaps = false,
      })
      vim.keymap.set("n", "<leader>kor", "<cmd>KittyOpenRunner<CR>", {})
      vim.keymap.set("n", "<leader>krc", "<cmd>KittyRunCommand<CR>", {})
      vim.keymap.set("n", "<leader>krr", "<cmd>KittyReRunCommand<CR>", {})
      vim.keymap.set("n", "<leader>kcr", "<cmd>KittyClearRunner<CR>", {})
      vim.keymap.set("n", "<leader>ksl", "<cmd>KittySendLines<CR>", {})
      vim.keymap.set("v", "<leader>ksl", "<cmd>KittySendLines<CR>", {})
      vim.keymap.set("n", "<leader>kkr", "<cmd>KittyKillRunner<CR>", {})
    end,
  })

  use({
    "nvim-lualine/lualine.nvim",
    config = function()
      local yaml_companion = require("yaml-companion")
      local function get_current_yaml_schema()
        local yaml_schema = yaml_companion.get_buf_schema(0)
        local schema_name = yaml_schema and yaml_schema.result[1].name or "none"

        if schema_name == "none" then
          return ""
        end

        return schema_name
      end

      require("lualine").setup({
        options = {
          icons_enabled = false,
          theme = "auto",
          component_separators = { left = "", right = "" },
          section_separators = { left = "", right = "" },
          disabled_filetypes = {},
          always_divide_middle = true,
          globalstatus = true,
        },
        sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {
            "branch",
            { "filename", path = 1, shorting_target = 20 },
          },
          lualine_x = {
            { get_current_yaml_schema },
            { "diagnostics", sources = { "nvim_diagnostic" }, colored = true },
            "filetype",
            "progress",
            "location",
          },
          lualine_y = {},
          lualine_z = {},
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = { "filename" },
          lualine_x = { "location" },
          lualine_y = {},
          lualine_z = {},
        },
        tabline = {},
        extensions = {},
      })
    end,
  })

  use({
    "marko-cerovac/material.nvim",
    config = function()
      vim.g.material_style = "darker"
      require("material").setup({
        disable = {
          colored_cursor = true,
        },
        contrast = {
          floating_windows = true,
        },
        lualine_style = "stealth",
        high_visibility = {
          darker = true,
        },
        async_loading = false,
        custom_highlights = {
          TelescopeNormal = { guibg = "bg" },
          TelescopePrompt = { guibg = "bg" },
          TelescopePromptBorder = { guibg = "bg" },
          TelescopePreviewBorder = { guibg = "bg" },
          TelescopeResultsBorder = { guibg = "bg" },
          TelescopePromptPreview = { guibg = "bg" },
          TelescopeBorder = { guibg = "bg" },
        },
      })
      vim.cmd("colorscheme material")
    end,
  })

  use({
    "nvim-telescope/telescope.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
      "nvim-telescope/telescope-ui-select.nvim",
    },
    config = function()
      local telescope = require("telescope")
      local opts = {
        disable_devicons = true,
        layout_config = {
          prompt_position = "top",
        },
      }

      telescope.setup({
        pickers = {
          find_files = opts,
          buffers = opts,
          oldfiles = opts,
          grep_string = opts,
          live_grep = opts,
        },
        extensions = {
          fzf = {
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
          },
          ["ui-select"] = opts,
        },
      })

      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")

      local builtins = require("telescope.builtin")

      vim.keymap.set("n", "<leader><leader>", builtins.find_files, {})
      vim.keymap.set("n", "<leader>fr", builtins.oldfiles, {})
      vim.keymap.set("n", "<leader>bb", builtins.buffers, {})
      vim.keymap.set("n", "<leader>g", builtins.live_grep, {})
      vim.keymap.set("n", "<leader>sw", builtins.diagnostics, {})
      vim.keymap.set("n", "<leader>so", builtins.lsp_workspace_symbols, {})
    end,
  })

  use({
    "ahmedkhalf/project.nvim",
    config = function()
      require("project_nvim").setup({})
    end,
  })

  use({
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "c",
          "lua",
          "python",
          "yaml",
          "bash",
          "comment",
          "typescript",
          "javascript",
          "tsx",
          "markdown",
        },
        highlight = {
          enable = true,
          -- disable = { "javascript", "typescript", "tsx" },
        },
      })
    end,
  })

  use({
    "numToStr/Comment.nvim",
    requires = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      require("Comment").setup({
        opleader = {
          line = "gc",
          block = "gb",
        },
        mappings = {
          basic = true,
          extra = true,
        },
      })
    end,
  })

  use({
    "neovim/nvim-lspconfig",
    requires = {
      "jose-elias-alvarez/null-ls.nvim",
      "jose-elias-alvarez/typescript.nvim",
      "someone-stole-my-name/yaml-companion.nvim",
      {
        "j-hui/fidget.nvim",
        config = function()
          require("fidget").setup({
            window = {
              relative = "editor",
              blend = 0,
            },
          })
        end,
      },
    },
    config = function()
      local lsp_conf = require("lspconfig")

      local common_on_attach = function(client, bufnr)
        vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

        -- usually not using lsp to format
        client.server_capabilities.document_formatting = false

        local opts = { buffer = bufnr }

        vim.keymap.set("i", "<C-b>", vim.lsp.buf.signature_help, opts)
        vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
        vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
        vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
        vim.keymap.set("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, opts)
        vim.keymap.set("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, opts)
        vim.keymap.set("n", "<leader>D", vim.lsp.buf.type_definition, opts)
        vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
        vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
        vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
        vim.keymap.set("n", "<leader>d", function()
          vim.diagnostic.open_float(nil, { scope = "line" })
        end, opts)
        vim.keymap.set("n", "[d", function()
          vim.diagnostic.goto_prev({ float = {} })
        end, opts)
        vim.keymap.set("n", "]d", function()
          vim.diagnostic.goto_next({ float = {} })
        end, opts)
      end

      local common_capabilities = require("cmp_nvim_lsp").update_capabilities(
        vim.lsp.protocol.make_client_capabilities()
      )

      local bind_lsp_format = function(bufnr)
        vim.keymap.set("n", "<leader>z", function()
          vim.lsp.buf.format({ async = true })
        end, { buffer = bufnr })
      end

      local null_ls_conf = require("null-ls")

      null_ls_conf.setup({
        sources = {
          null_ls_conf.builtins.formatting.prettier.with({
            filetypes = { "yaml", "json", "html", "css" },
          }),
          null_ls_conf.builtins.formatting.stylua.with({
            filetypes = { "lua" },
          }),
          null_ls_conf.builtins.formatting.black.with({
            filetypes = { "python" },
          }),
          null_ls_conf.builtins.formatting.shfmt.with({
            filetypes = { "sh" },
            args = { "-i", "2" },
          }),
        },
        on_attach = function(_, bufnr)
          bind_lsp_format(bufnr)
        end,
      })

      lsp_conf.eslint.setup({
        on_attach = function(_, bufnr)
          vim.keymap.set("n", "<leader>z", "<cmd>EslintFixAll<CR>", { buffer = bufnr, silent = true })
        end,
        settings = {
          codeAction = {
            disableRuleComment = {
              enable = false,
            },
            showDocumentation = {
              enable = false,
            },
          },
        },
      })

      require("typescript").setup({
        server = {
          on_attach = function(client, bufnr)
            common_on_attach(client, bufnr)

            local opts = { buffer = bufnr, silent = true }
            vim.keymap.set("n", "tor", ":TypescriptOrganizeImports<CR>", opts)
            vim.keymap.set("n", "trn", ":TypescriptRenameFile<CR>", opts)
            vim.keymap.set("n", "tia", ":TypescriptAddMissingImports<CR>", opts)
          end,
          capabilities = common_capabilities,
        },
      })

      lsp_conf.yamlls.setup(require("yaml-companion").setup({
        lspconfig = {
          on_attach = function(client, bufnr)
            common_on_attach(client, bufnr)

            vim.api.nvim_create_user_command("YamlSchema", require("yaml-companion").open_ui_select, { nargs = 0 })
          end,
          capabilities = common_capabilities,
          settings = {
            redhat = {
              telemetry = {
                enabled = false,
              },
            },
            schemaDownload = {
              enable = true,
            },
          },
        },
      }))

      lsp_conf.clangd.setup({
        on_attach = function(client, bufnr)
          common_on_attach(client, bufnr)

          client.server_capabilities.document_formatting = true
          bind_lsp_format(bufnr)
        end,
        capabilities = common_capabilities,
      })

      local basic_servers = { "pyright", "jsonls", "bashls" }
      for _, server in ipairs(basic_servers) do
        lsp_conf[server].setup({
          on_attach = common_on_attach,
          capabilities = common_capabilities,
        })
      end

      local runtime_path = vim.split(package.path, ";")
      table.insert(runtime_path, "lua/?.lua")
      table.insert(runtime_path, "lua/?/init.lua")

      lsp_conf.sumneko_lua.setup({
        cmd = { "lua-language-server" },
        on_attach = common_on_attach,
        capabilities = common_capabilities,
        settings = {
          Lua = {
            runtime = {
              version = "LuaJIT",
              path = runtime_path,
            },
            diagnostics = {
              globals = { "vim", "love" },
            },
            workspace = {
              library = vim.api.nvim_get_runtime_file("", true),
            },
            telemetry = {
              enable = false,
            },
          },
        },
      })

      vim.diagnostic.config({
        virtual_text = true,
        signs = true,
        update_in_insert = false,
      })
    end,
  })

  use({
    "L3MON4D3/LuaSnip",
    config = function()
      vim.cmd([[
        inoremap <silent><expr> <C-j> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<C-j>' 
        inoremap <silent> <C-k> <cmd>lua require'luasnip'.jump(-1)<Cr>
        snoremap <silent> <C-j> <cmd>lua require('luasnip').jump(1)<Cr>
        snoremap <silent> <C-k> <cmd>lua require('luasnip').jump(-1)<Cr>
      ]])

      local function load_custom_tsx_snip()
        local ls = require("luasnip")
        local fmt = require("luasnip.extras.fmt").fmt
        local s = ls.snippet
        local i = ls.insert_node
        local f = ls.function_node
        local d = ls.dynamic_node
        local sn = ls.snippet_node
        local rep = require("luasnip.extras").rep

        -- Get a list of  the property names given an `interface_declaration`
        -- treesitter *tsx* node.
        -- Ie, if the treesitter node represents:
        --   interface {
        --     prop1: string;
        --     prop2: number;
        --   }
        -- Then this function would return `{"prop1", "prop2"}
        ---@param id_node {} Stands for "interface declaration node"
        ---@return string[]
        local function get_prop_names(id_node)
          local object_type_node = id_node:child(2)
          if object_type_node:type() ~= "object_type" then
            return {}
          end

          local prop_names = {}

          for prop_signature in object_type_node:iter_children() do
            if prop_signature:type() == "property_signature" then
              local prop_iden = prop_signature:child(0)
              local prop_name = vim.treesitter.query.get_node_text(prop_iden, 0)
              prop_names[#prop_names + 1] = prop_name
            end
          end

          return prop_names
        end

        ls.add_snippets("typescriptreact", {
          s(
            "c",
            fmt(
              [[
{}interface {}Props {{
  {}
}}

{}function {}({{{}}}: {}Props) {{
  {}
}}
]],
              {
                i(1, "export "),

                -- Initialize component name to file name
                d(2, function(_, snip)
                  return sn(nil, {
                    i(1, vim.fn.substitute(snip.env.TM_FILENAME, "\\..*$", "", "g")),
                  })
                end, { 1 }),
                i(3, "// props"),
                rep(1),
                rep(2),
                f(function(_, snip, _)
                  local pos_begin = snip.nodes[6].mark:pos_begin()
                  local pos_end = snip.nodes[6].mark:pos_end()
                  local parser = vim.treesitter.get_parser(0, "tsx")
                  local tstree = parser:parse()

                  local node = tstree[1]
                    :root()
                    :named_descendant_for_range(pos_begin[1], pos_begin[2], pos_end[1], pos_end[2])

                  while node ~= nil and node:type() ~= "interface_declaration" do
                    node = node:parent()
                  end

                  if node == nil then
                    return ""
                  end

                  -- `node` is now surely of type "interface_declaration"
                  local prop_names = get_prop_names(node)

                  -- Does this lua->vimscript->lua thing cause a slow down? Dunno.
                  return vim.fn.join(prop_names, ", ")
                end, { 3 }),
                rep(2),
                i(5, "return <div></div>"),
              }
            )
          ),
        })
      end

      load_custom_tsx_snip()
    end,
  })

  use({
    "hrsh7th/nvim-cmp",
    requires = { "hrsh7th/cmp-buffer", "hrsh7th/cmp-nvim-lsp", "saadparwaiz1/cmp_luasnip" },
    config = function()
      local cmp = require("cmp")
      cmp.setup({
        completion = {
          autocomplete = false,
        },
        snippet = {
          expand = function(args)
            require("luasnip").lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-y>"] = cmp.mapping.confirm({ select = true }),
          ["<C-Space>"] = cmp.mapping.complete(),
          -- ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-e>"] = cmp.mapping.abort(),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "luasnip" },
          { name = "buffer", keyword_length = 4 },
        }),
      })
    end,
  })

  if packer_bootstrap ~= nil then
    require("packer").sync()
  end
end)

-- options
local opt = vim.opt

opt.termguicolors = true
opt.undofile = true
opt.hidden = true
opt.splitright = true
opt.splitbelow = true
opt.wrap = false
opt.signcolumn = "yes"
opt.tabstop = 2
opt.shiftwidth = 2
opt.expandtab = true
opt.number = true
opt.relativenumber = true
opt.mouse = "a"
opt.completeopt = { "menu", "menuone", "noselect" }
opt.foldenable = false
opt.ignorecase = true
opt.smartcase = true
opt.shortmess:append("c")
opt.guicursor = ""

vim.g.c_syntax_for_h = true
vim.g.loaded_ruby_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_node_provider = 0

-- mappings
vim.keymap.set({ "n", "v" }, "<space>", "<Nop>", {})
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.keymap.set("n", "<esc>", "<cmd>noh<CR>", {})
vim.keymap.set("t", "<esc><esc>", "<c-\\><c-n>", {})
vim.keymap.set("n", "<leader>lo", "<cmd>copen<CR>", {})
vim.keymap.set("n", "<leader>lc", "<cmd>cclose<CR>", {})
vim.keymap.set("n", "<c-j>", "<cmd>cnext<CR>", {})
vim.keymap.set("n", "<c-k>", "<cmd>cprev<CR>", {})
vim.keymap.set("n", "<leader>n", [[<cmd>set nu! rnu!<CR>]], {})

-- autocmds
local term_group = vim.api.nvim_create_augroup("Terminal", { clear = true })

vim.api.nvim_create_autocmd("TermOpen", {
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
    vim.opt_local.signcolumn = "no"
    vim.cmd("startinsert")
  end,
  group = term_group,
  pattern = "*",
})

local yank_highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })

vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = yank_highlight_group,
  pattern = "*",
})
