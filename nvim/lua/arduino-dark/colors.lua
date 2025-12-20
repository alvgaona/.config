-- Arduino Dark color palette

local colors = {
  -- Base colors
  bg = "#0D1B1E", -- Dark teal-black background
  fg = "#B2EBF2", -- Light cyan for text
  accent = "#00D3CF", -- Bright Arduino cyan

  -- Normal terminal colors
  black = "#263238", -- Dark blue-gray
  red = "#E57373", -- Soft red for errors
  green = "#00897B", -- Dark teal-green
  yellow = "#00838F", -- Dark cyan
  blue = "#00ACC1", -- Teal-cyan blue
  magenta = "#0097A7", -- Standard teal
  cyan = "#00979D", -- Arduino teal
  white = "#B2DFDB", -- Teal-tinted light gray

  -- Bright terminal colors
  bright_black = "#4A5F61", -- Teal-tinted dark gray
  bright_red = "#FF5252", -- Bright red (Arduino error color)
  bright_green = "#00E5CC", -- Bright teal-green
  bright_yellow = "#26C6DA", -- Bright cyan
  bright_blue = "#00BCD4", -- Bright teal-cyan
  bright_magenta = "#26C6DA", -- Bright teal
  bright_cyan = "#00E5FF", -- Bright Arduino cyan
  bright_white = "#E0F7FA", -- Cyan-tinted white

  -- UI elements
  bg_darker = "#0A1416", -- Slightly darker background
  bg_lighter = "#152B2F", -- Slightly lighter background
  bg_highlight = "#1A3338", -- Highlighted background
  comment = "#4A6E73", -- Muted cyan for comments
  selection = "#1A3338", -- Selection background
  border = "#00979D", -- Border color (Arduino teal)

  -- Diff backgrounds (semi-transparent effect simulated)
  diff_add = "#0D211F", -- Dark teal-green for added lines
  diff_change = "#0D1D20", -- Dark cyan for changed lines
  diff_delete = "#211517", -- Dark red for deleted lines
  diff_text = "#0F2023", -- Slightly brighter cyan for changed text
}

-- Semantic color aliases
colors.error = colors.bright_red
colors.warning = colors.bright_yellow
colors.info = colors.bright_blue
colors.hint = colors.bright_cyan
colors.success = colors.bright_green

return colors
