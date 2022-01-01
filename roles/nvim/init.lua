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
    "LnL7/vim-nix",
    ft = { "nix" },
  })

  use({
    "chemzqm/vim-jsx-improve",
  })

  use({
    "dkarter/bullets.vim",
    config = function()
      vim.g.bullets_enabled_file_types = { "markdown", "text", "gitcommit", "scratch" }
    end,
  })

  use({
    "nvim-lualine/lualine.nvim",
    requires = { "arkav/lualine-lsp-progress" },
    config = function()
      require("lualine").setup({
        options = {
          icons_enabled = false,
          theme = "auto",
          component_separators = { left = "", right = "" },
          section_separators = { left = "", right = "" },
          disabled_filetypes = {},
          always_divide_middle = true,
        },
        sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = { "branch", { "filename", path = 1 } },
          lualine_x = {
            {
              "lsp_progress",
              display_components = { "lsp_client_name", { "title", "percentage", "message" } },
            },
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
    "catppuccin/nvim",
    as = "catpuccin",
    config = function()
      require("catppuccin").setup({
        styles = {
          comments = "italic",
          functions = "NONE",
          keywords = "NONE",
          strings = "NONE",
          variables = "NONE",
        },
        integrations = {
          telescope = true,
          markdown = true,
          cmp = true,
        },
      })
      vim.cmd("colorscheme catppuccin")
    end,
  })

  use({
    "nvim-telescope/telescope.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-fzf-native.nvim",
    },
    config = function()
      local telescope = require("telescope")

      telescope.setup({
        pickers = {
          find_files = { theme = "dropdown", disable_devicons = true },
          buffers = { theme = "dropdown", disable_devicons = true },
          oldfiles = { theme = "dropdown", disable_devicons = true },
          grep_string = { theme = "dropdown", disable_devicons = true },
          live_grep = { theme = "dropdown", disable_devicons = true },
        },
        extensions = {
          fzf = {
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
          },
        },
      })

      telescope.load_extension("fzf")

      vim.api.nvim_set_keymap(
        "n",
        "<leader><leader>",
        "<cmd>lua require('telescope.builtin').find_files()<cr>",
        { noremap = true, silent = true }
      )
      vim.api.nvim_set_keymap(
        "n",
        "<leader>fh",
        "<cmd>lua require('telescope.builtin').find_files()<cr>",
        { noremap = true, silent = true }
      )
      vim.api.nvim_set_keymap(
        "n",
        "<leader>fr",
        "<cmd>lua require('telescope.builtin').oldfiles()<cr>",
        { noremap = true, silent = true }
      )
      vim.api.nvim_set_keymap(
        "n",
        "<leader>bb",
        "<cmd>lua require('telescope.builtin').buffers()<cr>",
        { noremap = true, silent = true }
      )
      vim.api.nvim_set_keymap(
        "n",
        "<leader>g",
        "<cmd>lua require('telescope.builtin').live_grep()<cr>",
        { noremap = true, silent = true }
      )
      vim.api.nvim_set_keymap("n", "<leader>sw", "<cmd>Telescope diagnostics<cr>", { noremap = true, silent = true })
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
          "nix",
          "yaml",
          "bash",
          "comment",
          "typescript",
          "javascript",
          "markdown",
        },
        highlight = {
          enable = true,
          disable = { "javascript", "typescript" },
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
      "jose-elias-alvarez/nvim-lsp-ts-utils",
      "jose-elias-alvarez/null-ls.nvim",
      "stevearc/dressing.nvim",
    },
    config = function()
      local nvim_lsp = require("lspconfig")

      local common_on_attach = function(client, bufnr)
        local function buf_set_keymap(...)
          vim.api.nvim_buf_set_keymap(bufnr, ...)
        end
        local function buf_set_option(...)
          vim.api.nvim_buf_set_option(bufnr, ...)
        end

        buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

        local opts = { noremap = true, silent = true }

        -- usually not using lsp to format
        client.resolved_capabilities.document_formatting = false

        buf_set_keymap("i", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
        buf_set_keymap("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
        buf_set_keymap("n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
        buf_set_keymap("n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts)
        buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
        buf_set_keymap("n", "<leader>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
        buf_set_keymap("n", "<leader>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
        buf_set_keymap("n", "<leader>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
        buf_set_keymap("n", "<leader>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
        buf_set_keymap("n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
        buf_set_keymap("n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
        buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
        buf_set_keymap("n", "<leader>d", "<cmd>lua vim.diagnostic.open_float(nil, { scope='line' })<CR>", opts)
        buf_set_keymap("n", "[d", "<cmd>lua vim.diagnostic.goto_prev({ float = {} })<CR>", opts)
        buf_set_keymap("n", "]d", "<cmd>lua vim.diagnostic.goto_next({ float = {} })<CR>", opts)
      end

      local common_capabilities = require("cmp_nvim_lsp").update_capabilities(
        vim.lsp.protocol.make_client_capabilities()
      )

      local null_ls = require("null-ls")

      null_ls.setup({
        sources = {
          null_ls.builtins.formatting.prettier.with({
            filetypes = { "yaml", "json", "html", "css" },
          }),
          null_ls.builtins.formatting.stylua.with({
            filetypes = { "lua" },
          }),
          null_ls.builtins.formatting.nixfmt.with({
            filetypes = { "nix" },
          }),
          null_ls.builtins.formatting.black.with({
            filetypes = { "python" },
          }),
          null_ls.builtins.formatting.isort.with({
            filetypes = { "python" },
          }),
        },
        on_attach = function(_, bufnr)
          vim.api.nvim_buf_set_keymap(
            bufnr,
            "n",
            "<leader>z",
            "<cmd>lua vim.lsp.buf.formatting()<CR>",
            { noremap = true, silent = true }
          )
        end,
      })

      nvim_lsp.eslint.setup({
        on_attach = function(_, bufnr)
          vim.api.nvim_buf_set_keymap(
            bufnr,
            "n",
            "<leader>z",
            "<cmd>EslintFixAll<CR>",
            { noremap = true, silent = true }
          )
        end,
        settings = {
          codeAction = {
            disableRuleComment = {
              enable = false,
              location = "separateLine",
            },
            showDocumentation = {
              enable = false,
            },
          },
        },
        flags = {
          debounce_text_changes = 200,
        },
      })

      nvim_lsp.tsserver.setup({
        on_attach = function(client, bufnr)
          common_on_attach(client, bufnr)

          local ts_utils = require("nvim-lsp-ts-utils")

          ts_utils.setup({
            enable_import_on_completion = true,
            filter_out_diagnostics_by_code = { 80001 },
            filter_out_diagnostics_by_severity = { "hint" },
          })

          -- required to fix code action ranges
          ts_utils.setup_client(client)

          vim.api.nvim_buf_set_keymap(bufnr, "n", "tor", ":TSLspOrganize<CR>", { silent = true })
          vim.api.nvim_buf_set_keymap(bufnr, "n", "tqf", ":TSLspFixCurrent<CR>", { silent = true })
          vim.api.nvim_buf_set_keymap(bufnr, "n", "trn", ":TSLspRenameFile<CR>", { silent = true })
          vim.api.nvim_buf_set_keymap(bufnr, "n", "tia", ":TSLspImportAll<CR>", { silent = true })
        end,
        capabilities = common_capabilities,
        flags = {
          debounce_text_changes = 200,
        },
      })

      nvim_lsp.yamlls.setup({
        on_attach = common_on_attach,
        capabilities = common_capabilities,
        settings = {
          yaml = {
            customTags = { "!Ref", "!ImportValue" },
          },
        },
        flags = {
          debounce_text_changes = 200,
        },
      })

      nvim_lsp.pyright.setup({
        on_attach = common_on_attach,
        capabilities = common_capabilities,
        flags = {
          debounce_text_changes = 200,
        },
      })

      nvim_lsp.jsonls.setup({
        on_attach = common_on_attach,
        capabilities = common_capabilities,
        flags = {
          debounce_text_changes = 200,
        },
      })

      nvim_lsp.clangd.setup({
        on_attach = function(client, bufnr)
          common_on_attach(client, bufnr)

          client.resolved_capabilities.document_formatting = true
          vim.api.nvim_buf_set_keymap(
            bufnr,
            "n",
            "<leader>z",
            "<cmd>lua vim.lsp.buf.formatting()<CR>",
            { noremap = true, silent = true }
          )
        end,
        capabilities = common_capabilities,
        flags = {
          debounce_text_changes = 200,
        },
      })

      local runtime_path = vim.split(package.path, ";")
      table.insert(runtime_path, "lua/?.lua")
      table.insert(runtime_path, "lua/?/init.lua")

      nvim_lsp.sumneko_lua.setup({
        cmd = { "lua-language-server" },
        on_attach = common_on_attach,
        capabilities = common_capabilities,
        settings = {
          Lua = {
            runtime = {
              -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
              version = "LuaJIT",
              -- Setup your lua path
              path = runtime_path,
            },
            diagnostics = {
              globals = { "vim", "love" },
            },
            workspace = {
              -- Make the server aware of Neovim runtime files
              library = vim.api.nvim_get_runtime_file("", true),
              maxPreload = 2000,
              preloadFileSize = 1000,
              checkThirdParty = false,
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
        imap <silent><expr> <Tab> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<Tab>' 
        inoremap <silent> <S-Tab> <cmd>lua require'luasnip'.jump(-1)<Cr>
        snoremap <silent> <Tab> <cmd>lua require('luasnip').jump(1)<Cr>
        snoremap <silent> <S-Tab> <cmd>lua require('luasnip').jump(-1)<Cr>
      ]])
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
        mapping = {
          ["<C-y>"] = cmp.mapping.confirm({ select = true }),
          ["<C-Space>"] = cmp.mapping.complete(),
        },
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "luasnip" },
          { name = "buffer", keyword_length = 4 },
        }),
      })
    end,
  })

  if packer_bootstrap then
    require("packer").sync()
  end
end)

-- general settings
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

vim.cmd([[
augroup YankHighlight
  autocmd!
  autocmd TextYankPost * silent! lua vim.highlight.on_yank()
augroup end
]])

-- mappings
vim.g.mapleader = " "
vim.api.nvim_set_keymap("n", "<esc>", "<cmd>noh<CR>", { noremap = false, silent = true })
vim.api.nvim_set_keymap("n", "<leader>lo", "<cmd>copen<CR>", { noremap = false, silent = true })
vim.api.nvim_set_keymap("n", "<leader>lc", "<cmd>cclose<CR>", { noremap = false, silent = true })
vim.api.nvim_set_keymap("n", "<c-j>", "<cmd>cnext<CR>", { noremap = false, silent = true })
vim.api.nvim_set_keymap("n", "<c-k>", "<cmd>cprev<CR>", { noremap = false, silent = true })
vim.api.nvim_set_keymap("n", "<leader>n", [[<cmd>set nu! rnu!<CR>]], { noremap = false, silent = true })

vim.cmd([[
augroup Terminal
  autocmd!
  au TermOpen * setlocal nonu nornu signcolumn=no | startinsert
augroup end
]])
