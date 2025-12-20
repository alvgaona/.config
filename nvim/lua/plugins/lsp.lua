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
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {},
    },
  },
}
