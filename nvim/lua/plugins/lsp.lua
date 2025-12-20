return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        ruff = false,
        ruff_lsp = false,
      },
    },
  },
  {
    "mason-org/mason.nvim",
    opts = {
      ensure_installed = {},
    },
  },
}
