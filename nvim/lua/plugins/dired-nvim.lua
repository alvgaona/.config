return {
  dir = "/Users/alvaro/git/dired.nvim",
  -- "alvgaona/dired.nvim",
  dependencies = { "MunifTanjim/nui.nvim" },
  cmd = "Dired",
  keys = {
    { "-", "<cmd>Dired<cr>", desc = "Open dired" },
  },
  config = function()
    require("dired").setup({
      path_separator = "/",
      show_banner = false,
      show_icons = false,
      show_hidden = true,
      show_dot_dirs = true,
      show_colors = true,
    })
  end,
}
