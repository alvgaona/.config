local M = {}

function M.setup(colors, opts)
  opts = opts or {}

  local highlights = {
    -- Base highlights
    Normal = { fg = colors.fg, bg = opts.transparent and "NONE" or colors.bg },
    NormalFloat = { fg = colors.fg, bg = opts.transparent and "NONE" or colors.bg_lighter },
    NormalNC = { fg = colors.fg, bg = opts.transparent and "NONE" or colors.bg },

    -- UI Elements
    ColorColumn = { bg = colors.bg_lighter },
    Cursor = { fg = colors.bg, bg = colors.fg },
    CursorLine = { bg = colors.bg_lighter },
    CursorLineNr = { fg = colors.accent, bold = true },
    LineNr = { fg = colors.comment },
    SignColumn = { fg = colors.fg, bg = opts.transparent and "NONE" or colors.bg },
    StatusLine = { fg = colors.fg, bg = colors.bg_lighter },
    StatusLineNC = { fg = colors.comment, bg = colors.bg_darker },
    VertSplit = { fg = colors.border },
    WinSeparator = { fg = colors.border },

    -- Selections and Search
    Visual = { bg = colors.selection },
    VisualNOS = { bg = colors.selection },
    Search = { fg = colors.bg, bg = colors.bright_yellow },
    IncSearch = { fg = colors.bg, bg = colors.accent },
    CurSearch = { fg = colors.bg, bg = colors.accent },

    -- Popup Menu
    Pmenu = { fg = colors.fg, bg = colors.bg_lighter },
    PmenuSel = { fg = colors.bg, bg = colors.accent },
    PmenuSbar = { bg = colors.bg_highlight },
    PmenuThumb = { bg = colors.cyan },

    -- Syntax highlighting
    Comment = { fg = colors.comment, italic = opts.italic_comments ~= false },
    Constant = { fg = colors.bright_magenta },
    String = { fg = colors.bright_green },
    Character = { fg = colors.bright_green },
    Number = { fg = colors.bright_magenta },
    Boolean = { fg = colors.bright_magenta },
    Float = { fg = colors.bright_magenta },

    Identifier = { fg = colors.bright_cyan },
    Function = { fg = colors.bright_blue },

    Statement = { fg = colors.cyan },
    Conditional = { fg = colors.cyan },
    Repeat = { fg = colors.cyan },
    Label = { fg = colors.cyan },
    Operator = { fg = colors.accent },
    Keyword = { fg = colors.cyan },
    Exception = { fg = colors.bright_red },

    PreProc = { fg = colors.magenta },
    Include = { fg = colors.magenta },
    Define = { fg = colors.magenta },
    Macro = { fg = colors.magenta },
    PreCondit = { fg = colors.magenta },

    Type = { fg = colors.bright_yellow },
    StorageClass = { fg = colors.cyan },
    Structure = { fg = colors.bright_yellow },
    Typedef = { fg = colors.bright_yellow },

    Special = { fg = colors.accent },
    SpecialChar = { fg = colors.accent },
    Tag = { fg = colors.bright_blue },
    Delimiter = { fg = colors.fg },
    SpecialComment = { fg = colors.bright_cyan },
    Debug = { fg = colors.bright_red },

    Underlined = { underline = true },
    Ignore = { fg = colors.comment },
    Error = { fg = colors.error },
    Todo = { fg = colors.bg, bg = colors.bright_yellow, bold = true },

    -- Treesitter
    ["@variable"] = { fg = colors.fg },
    ["@variable.builtin"] = { fg = colors.bright_magenta },
    ["@variable.parameter"] = { fg = colors.bright_cyan },
    ["@variable.member"] = { fg = colors.bright_cyan },

    ["@constant"] = { fg = colors.bright_magenta },
    ["@constant.builtin"] = { fg = colors.bright_magenta },
    ["@constant.macro"] = { fg = colors.magenta },

    ["@string"] = { fg = colors.bright_green },
    ["@string.escape"] = { fg = colors.accent },
    ["@string.regexp"] = { fg = colors.bright_yellow },

    ["@character"] = { fg = colors.bright_green },
    ["@number"] = { fg = colors.bright_magenta },
    ["@boolean"] = { fg = colors.bright_magenta },
    ["@float"] = { fg = colors.bright_magenta },

    ["@function"] = { fg = colors.bright_blue },
    ["@function.builtin"] = { fg = colors.bright_blue },
    ["@function.macro"] = { fg = colors.magenta },
    ["@function.method"] = { fg = colors.bright_blue },

    ["@constructor"] = { fg = colors.bright_yellow },
    ["@operator"] = { fg = colors.accent },
    ["@keyword"] = { fg = colors.cyan },
    ["@keyword.function"] = { fg = colors.cyan },
    ["@keyword.operator"] = { fg = colors.accent },
    ["@keyword.return"] = { fg = colors.cyan },

    ["@conditional"] = { fg = colors.cyan },
    ["@repeat"] = { fg = colors.cyan },
    ["@exception"] = { fg = colors.bright_red },

    ["@type"] = { fg = colors.bright_yellow },
    ["@type.builtin"] = { fg = colors.bright_yellow },
    ["@type.qualifier"] = { fg = colors.cyan },

    ["@property"] = { fg = colors.bright_cyan },
    ["@attribute"] = { fg = colors.magenta },
    ["@namespace"] = { fg = colors.bright_yellow },

    ["@comment"] = { fg = colors.comment, italic = opts.italic_comments ~= false },
    ["@comment.todo"] = { fg = colors.bg, bg = colors.bright_yellow, bold = true },
    ["@comment.warning"] = { fg = colors.bg, bg = colors.warning, bold = true },
    ["@comment.error"] = { fg = colors.bg, bg = colors.error, bold = true },
    ["@comment.note"] = { fg = colors.bg, bg = colors.info, bold = true },

    ["@tag"] = { fg = colors.bright_blue },
    ["@tag.attribute"] = { fg = colors.bright_cyan },
    ["@tag.delimiter"] = { fg = colors.accent },

    ["@punctuation.delimiter"] = { fg = colors.fg },
    ["@punctuation.bracket"] = { fg = colors.fg },
    ["@punctuation.special"] = { fg = colors.accent },

    -- LSP
    DiagnosticError = { fg = colors.error },
    DiagnosticWarn = { fg = colors.warning },
    DiagnosticInfo = { fg = colors.info },
    DiagnosticHint = { fg = colors.hint },

    DiagnosticUnderlineError = { undercurl = true, sp = colors.error },
    DiagnosticUnderlineWarn = { undercurl = true, sp = colors.warning },
    DiagnosticUnderlineInfo = { undercurl = true, sp = colors.info },
    DiagnosticUnderlineHint = { undercurl = true, sp = colors.hint },

    LspReferenceText = { bg = colors.bg_highlight },
    LspReferenceRead = { bg = colors.bg_highlight },
    LspReferenceWrite = { bg = colors.bg_highlight },

    -- Git signs
    GitSignsAdd = { fg = colors.success },
    GitSignsChange = { fg = colors.warning },
    GitSignsDelete = { fg = colors.error },

    -- Diff
    DiffAdd = { bg = colors.diff_add },
    DiffChange = { bg = colors.diff_change },
    DiffDelete = { fg = colors.error, bg = colors.diff_delete },
    DiffText = { bg = colors.diff_text },

    -- Telescope
    TelescopeBorder = { fg = colors.border },
    TelescopePromptBorder = { fg = colors.accent },
    TelescopePromptTitle = { fg = colors.accent, bold = true },
    TelescopeSelection = { bg = colors.bg_highlight },
    TelescopeSelectionCaret = { fg = colors.accent },

    -- Neo-tree
    NeoTreeNormal = { fg = colors.fg, bg = opts.transparent and "NONE" or colors.bg },
    NeoTreeNormalNC = { fg = colors.fg, bg = opts.transparent and "NONE" or colors.bg },
    NeoTreeDirectoryIcon = { fg = colors.bright_blue },
    NeoTreeDirectoryName = { fg = colors.bright_blue },
    NeoTreeGitAdded = { fg = colors.success },
    NeoTreeGitModified = { fg = colors.warning },
    NeoTreeGitDeleted = { fg = colors.error },
    NeoTreeGitUntracked = { fg = colors.bright_green },

    -- WhichKey
    WhichKey = { fg = colors.accent },
    WhichKeyGroup = { fg = colors.bright_blue },
    WhichKeyDesc = { fg = colors.fg },
    WhichKeySeparator = { fg = colors.comment },
    WhichKeyFloat = { bg = colors.bg_lighter },

    -- Notify
    NotifyBackground = { bg = colors.bg },
    NotifyERRORBorder = { fg = colors.error },
    NotifyWARNBorder = { fg = colors.warning },
    NotifyINFOBorder = { fg = colors.info },
    NotifyDEBUGBorder = { fg = colors.comment },
    NotifyTRACEBorder = { fg = colors.hint },

    -- IndentBlankline
    IblIndent = { fg = colors.bg_lighter },
    IblScope = { fg = colors.border },
  }

  -- Apply all highlights
  for group, opts in pairs(highlights) do
    vim.api.nvim_set_hl(0, group, opts)
  end
end

return M
