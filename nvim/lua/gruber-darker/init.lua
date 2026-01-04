-- Gruber Darker colorscheme for Neovim
-- Based on the Emacs Gruber Darker theme by Alexey Kutepov

local M = {}

M.setup = function(opts)
  opts = opts or {}

  -- Default options
  opts = vim.tbl_extend("force", {
    transparent = false,
    italic_comments = true,
  }, opts)

  -- Clear existing highlights
  if vim.g.colors_name then
    vim.cmd("highlight clear")
  end

  if vim.fn.exists("syntax_on") then
    vim.cmd("syntax reset")
  end

  -- Set color name and enable 24-bit color
  vim.g.colors_name = "gruber-darker"
  vim.o.termguicolors = true

  -- Load colors and apply highlights
  local colors = require("gruber-darker.colors")
  require("gruber-darker.highlights").setup(colors, opts)

  -- Set terminal colors
  vim.g.terminal_color_0 = colors.black
  vim.g.terminal_color_1 = colors.red
  vim.g.terminal_color_2 = colors.green
  vim.g.terminal_color_3 = colors.yellow
  vim.g.terminal_color_4 = colors.blue
  vim.g.terminal_color_5 = colors.magenta
  vim.g.terminal_color_6 = colors.cyan
  vim.g.terminal_color_7 = colors.white
  vim.g.terminal_color_8 = colors.bright_black
  vim.g.terminal_color_9 = colors.bright_red
  vim.g.terminal_color_10 = colors.bright_green
  vim.g.terminal_color_11 = colors.bright_yellow
  vim.g.terminal_color_12 = colors.bright_blue
  vim.g.terminal_color_13 = colors.bright_magenta
  vim.g.terminal_color_14 = colors.bright_cyan
  vim.g.terminal_color_15 = colors.bright_white
end

return M
