-- packer bootstrap
local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
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
    "tpope/vim-fugitive",
  })

  use({
    "kylechui/nvim-surround",
    config = function()
      require("nvim-surround").setup({})
    end,
  })

  use({
    "https://gitlab.com/yorickpeterse/nvim-pqf",
    config = function()
      require("pqf").setup()
    end,
  })

  use({
    "numToStr/Fterm.nvim",
    config = function()
      local fterm = require("FTerm")

      vim.keymap.set({ "n", "t" }, "<A-t>", fterm.toggle)
    end,
  })

  use({
    "ggandor/leap.nvim",
    config = function()
      require("leap").set_default_keymaps()
    end,
  })

  use({
    "anuvyklack/hydra.nvim",
    config = function()
      local Hydra = require("hydra")

      Hydra({
        name = "Change / Resize Window",
        mode = { "n" },
        body = "<C-w>",
        heads = {
          -- resizing window
          { "<left>", "<C-w>3<" },
          { "<right>", "<C-w>3>" },
          { "<up>", "<C-w>2+" },
          { "<down>", "<C-w>2-" },

          -- equalize window sizes
          { "e", "<C-w>=" },

          -- close active window
          { "Q", ":q<cr>" },
          { "<C-q>", ":q<cr>" },

          -- exit this Hydra
          { "q", nil, { exit = true, nowait = true } },
          { ";", nil, { exit = true, nowait = true } },
          { "<Esc>", nil, { exit = true, nowait = true } },
        },
      })
    end,
  })

  use({
    "kyazdani42/nvim-tree.lua",
    config = function()
      require("nvim-tree").setup({
        hijack_netrw = false,
        renderer = {
          indent_markers = {
            enable = true,
          },
          icons = {
            show = {
              file = false,
              folder = false,
              folder_arrow = false,
              git = false,
            },
          },
        },
      })
      vim.keymap.set("n", "<leader>nt", "<cmd>NvimTreeToggle<CR>", {})
    end,
  })

  use({
    "nvim-lualine/lualine.nvim",
    config = function()
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
    "jacksonludwig/vim-moonfly-colors",
    config = function()
      vim.g.moonflyWinSeparator = 2
      vim.cmd.colorscheme("moonfly")
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
        defaults = opts,
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
      vim.keymap.set("n", "<leader>fv", builtins.git_files, {})
      vim.keymap.set("n", "<leader>bb", builtins.buffers, {})
      vim.keymap.set("n", "<leader>gg", builtins.live_grep, {})
      vim.keymap.set("n", "<leader>gb", builtins.current_buffer_fuzzy_find, {})
      vim.keymap.set("n", "<leader>Sw", builtins.diagnostics, {})
      vim.keymap.set("n", "<leader>So", builtins.lsp_workspace_symbols, {})

      vim.keymap.set("n", "<leader>vb", builtins.git_branches, {})
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
    requires = { "windwp/nvim-ts-autotag" },
    run = ":TSUpdate",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "c",
          "lua",
          "python",
          "yaml",
          "bash",
          -- "comment"
          "typescript",
          "javascript",
          -- "jsdoc",
          "tsx",
          "regex",
          "css",
          "html",
          "go",
        },
        highlight = {
          enable = true,
        },
        indent = {
          enable = true,
        },
        autotag = {
          enable = true,
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
    "j-hui/fidget.nvim",
    config = function()
      require("fidget").setup({
        text = {
          spinner = "dots",
        },
        window = {
          relative = "editor",
          blend = 0,
        },
        fmt = {
          task = function(_, _, _)
            -- don't show status message at all
            return nil
          end,
        },
      })
    end,
  })

  use({
    "neovim/nvim-lspconfig",
    requires = {
      "jose-elias-alvarez/null-ls.nvim",
      "jose-elias-alvarez/typescript.nvim",
    },
    config = function()
      local lsp_conf = require("lspconfig")

      local function common_on_attach(_, bufnr)
        vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

        local opts = { buffer = bufnr }

        vim.keymap.set("i", "<C-b>", vim.lsp.buf.signature_help, opts)
        vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
        vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
        vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
        vim.keymap.set("n", "<leader>Wa", vim.lsp.buf.add_workspace_folder, opts)
        vim.keymap.set("n", "<leader>Wr", vim.lsp.buf.remove_workspace_folder, opts)
        vim.keymap.set("n", "<leader>D", vim.lsp.buf.type_definition, opts)
        vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
        vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
        vim.keymap.set("v", "<leader>ca", vim.lsp.buf.range_code_action, opts)
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

      local lsp_util = require("vim.lsp.util")
      local bind_lsp_format = function(client, bufnr)
        vim.keymap.set("n", "<leader>z", function()
          local params = lsp_util.make_formatting_params({ async = true })
          client.request("textDocument/formatting", params, nil, bufnr)
        end, { buffer = bufnr })
      end

      local null_ls = require("null-ls")

      null_ls.setup({
        sources = {
          null_ls.builtins.formatting.prettier.with({
            filetypes = { "yaml", "json", "html", "css" },
          }),
          null_ls.builtins.formatting.stylua.with({
            filetypes = { "lua" },
          }),
        },
        on_attach = function(client, bufnr)
          bind_lsp_format(client, bufnr)
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

      lsp_conf.gopls.setup({
        on_attach = function(client, bufnr)
          common_on_attach(client, bufnr)
          bind_lsp_format(client, bufnr)
        end,
        capabilities = common_capabilities,
      })

      local basic_servers = { "jsonls" }
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
              globals = { "vim" },
            },
            workspace = {
              library = {
                vim.fn.expand("$VIMRUNTIME/lua"),
                vim.fn.stdpath("config") .. "/lua",
              },
            },
          },
        },
      })

      vim.diagnostic.config({
        virtual_text = false,
        signs = true,
      })
    end,
  })

  use({
    "L3MON4D3/LuaSnip",
    config = function()
      local luasnip = require("luasnip")

      require("jackson.snippets.snippets")

      vim.cmd([[
        inoremap <silent><expr> <C-j> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<C-j>' 
        inoremap <silent> <C-k> <cmd>lua require'luasnip'.jump(-1)<Cr>
        snoremap <silent> <C-j> <cmd>lua require('luasnip').jump(1)<Cr>
        snoremap <silent> <C-k> <cmd>lua require('luasnip').jump(-1)<Cr>
      ]])

      vim.keymap.set({ "i", "s" }, "<C-l>", function()
        if luasnip.choice_active() then
          luasnip.change_choice(1)
        end
      end)
    end,
  })

  use({
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup({
        disable_in_macro = true,
      })
    end,
  })

  use({
    "hrsh7th/nvim-cmp",
    requires = { "hrsh7th/cmp-buffer", "hrsh7th/cmp-nvim-lsp", "saadparwaiz1/cmp_luasnip" },
    config = function()
      local cmp = require("cmp")

      cmp.setup({
        snippet = {
          expand = function(args)
            require("luasnip").lsp_expand(args.body)
          end,
        },
        mapping = {
          ["<C-y>"] = cmp.mapping({ i = cmp.mapping.confirm({ select = true }) }),
          ["<C-n>"] = cmp.mapping({ i = cmp.mapping.select_next_item() }),
          ["<C-p>"] = cmp.mapping({ i = cmp.mapping.select_prev_item() }),
          ["<C-Space>"] = cmp.mapping({ i = cmp.mapping.complete() }),
          ["<A-p>"] = cmp.mapping({ i = cmp.mapping.scroll_docs(-4) }),
          ["<A-n>"] = cmp.mapping({ i = cmp.mapping.scroll_docs(4) }),
          ["<A-e>"] = cmp.mapping({ i = cmp.mapping.abort() }),
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

-- options
local opt = vim.opt

opt.termguicolors = true
opt.undofile = true
opt.hidden = true
opt.splitright = true
opt.splitbelow = true
opt.wrap = false
opt.signcolumn = "number"
opt.tabstop = 2
opt.shiftwidth = 2
opt.expandtab = true
opt.relativenumber = true
opt.completeopt = { "menuone", "noselect" }
opt.foldenable = false
opt.ignorecase = true
opt.smartcase = true
opt.smartindent = true
opt.shortmess:append("c")
opt.guicursor = ""

vim.g.loaded_ruby_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_node_provider = 0

-- Util functions
local function move_to_end_of_line()
  local row, _ = unpack(vim.api.nvim_win_get_cursor(0))

  vim.api.nvim_win_set_cursor(0, { row, vim.fn.col("$") - 1 })
end

local function move_to_beginning_of_line()
  local row, _ = unpack(vim.api.nvim_win_get_cursor(0))

  local first_non_whitespace_col = vim.fn.match(vim.fn.getline(tostring(row)), [[\S]])

  vim.api.nvim_win_set_cursor(0, { row, first_non_whitespace_col >= 0 and first_non_whitespace_col or 0 })
end

-- mappings
vim.keymap.set({ "n", "v" }, "<space>", "<Nop>", {})
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.keymap.set("n", "<esc>", "<cmd>noh<CR>", {})
vim.keymap.set("n", "<leader>lo", "<cmd>copen<CR>", {})
vim.keymap.set("n", "<leader>lc", "<cmd>cclose<CR>", {})
vim.keymap.set("n", "<c-j>", "<cmd>cnext<CR>", {})
vim.keymap.set("n", "<c-k>", "<cmd>cprev<CR>", {})
vim.keymap.set("n", "<leader>#", "<cmd>set rnu!<CR>", {})

vim.keymap.set("i", "<C-e>", move_to_end_of_line, {})
vim.keymap.set("i", "<C-a>", move_to_beginning_of_line, {})

-- autocmds
vim.api.nvim_create_autocmd("TermOpen", {
  callback = function()
    if not vim.startswith(vim.api.nvim_buf_get_name(0), "term://") then
      return
    end

    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
    vim.opt_local.signcolumn = "no"
    vim.cmd.startinsert()
  end,
  group = vim.api.nvim_create_augroup("Terminal", { clear = true }),
  pattern = "*",
})

vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
  pattern = "*",
})

vim.api.nvim_create_autocmd("BufEnter", {
  pattern = "COMMIT_EDITMSG",
  callback = function()
    vim.opt_local.spell = true
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
    if vim.fn.getline(1) == "" then
      vim.cmd("startinsert!")
    end
  end,
  group = vim.api.nvim_create_augroup("Git", { clear = true }),
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "qf",
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
  end,
  group = vim.api.nvim_create_augroup("QF", { clear = true }),
})

-- random gui stuff
if vim.fn.exists("g:neovide") == 1 then
  vim.opt.guifont = 'JetBrains Mono:h13'
  vim.g.neovide_cursor_animation_length = 0

  vim.g.gui_font_default_size = 13
  vim.g.gui_font_size = vim.g.gui_font_default_size
  vim.g.gui_font_face = "JetBrains Mono"

  RefreshGuiFont = function()
    vim.opt.guifont = string.format("%s:h%s",vim.g.gui_font_face, vim.g.gui_font_size)
  end

  ResizeGuiFont = function(delta)
    vim.g.gui_font_size = vim.g.gui_font_size + delta
    RefreshGuiFont()
  end

  ResetGuiFont = function()
    vim.g.gui_font_size = vim.g.gui_font_default_size
    RefreshGuiFont()
  end

  -- Call function on startup to set default value
  ResetGuiFont()

  -- Keymaps
  local opts = { noremap = true, silent = true }

  vim.keymap.set({'n', 'i'}, "<C-+>", function() ResizeGuiFont(1)  end, opts)
  vim.keymap.set({'n', 'i'}, "<C-->", function() ResizeGuiFont(-1) end, opts)
end
