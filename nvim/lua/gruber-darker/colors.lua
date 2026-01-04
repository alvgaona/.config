-- Gruber Darker color palette
-- https://github.com/rexim/gruber-darker-theme

local colors = {
  -- Base colors from Gruber Darker
  bg = "#181818",
  fg = "#e4e4ef",
  accent = "#96a6c8", -- niagara

  -- Normal terminal colors
  black = "#282828",
  red = "#f43841",
  green = "#73c936",
  yellow = "#ffdd33",
  blue = "#96a6c8", -- niagara
  magenta = "#9e95c7", -- wisteria
  cyan = "#95a99f", -- quartz
  white = "#e4e4ef",

  -- Bright terminal colors
  bright_black = "#484848",
  bright_red = "#ff4f58",
  bright_green = "#73c936",
  bright_yellow = "#ffdd33",
  bright_blue = "#96a6c8",
  bright_magenta = "#9e95c7",
  bright_cyan = "#95a99f",
  bright_white = "#f4f4ff",

  -- UI elements
  bg_darker = "#101010", -- bg-1
  bg_lighter = "#282828", -- bg+1
  bg_highlight = "#453d41", -- bg+2
  comment = "#565f73", -- niagara-1
  selection = "#453d41", -- bg+2
  border = "#52494e", -- bg+4

  -- Additional Gruber Darker colors
  brown = "#cc8c3c",
  niagara = "#96a6c8",
  niagara_1 = "#565f73",
  niagara_2 = "#303540",
  wisteria = "#9e95c7",
  quartz = "#95a99f",

  -- Diff backgrounds
  diff_add = "#1a2e1a",
  diff_change = "#1a1a2e",
  diff_delete = "#2e1a1a",
  diff_text = "#2e2e1a",
}

-- Semantic color aliases
colors.error = colors.bright_red
colors.warning = colors.yellow
colors.info = colors.niagara
colors.hint = colors.quartz
colors.success = colors.green

return colors
