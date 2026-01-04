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
    CursorLineNr = { fg = colors.yellow, bold = true },
    LineNr = { fg = colors.comment },
    SignColumn = { fg = colors.fg, bg = opts.transparent and "NONE" or colors.bg },
    StatusLine = { fg = colors.fg, bg = colors.bg_lighter },
    StatusLineNC = { fg = colors.comment, bg = colors.bg_darker },
    VertSplit = { fg = colors.border },
    WinSeparator = { fg = colors.border },
    Folded = { fg = colors.niagara_1, bg = colors.bg_lighter },
    FoldColumn = { fg = colors.niagara_1 },
    MatchParen = { fg = colors.fg, bg = colors.bg_highlight, bold = true },
    NonText = { fg = colors.niagara_2 },
    SpecialKey = { fg = colors.niagara_2 },
    Title = { fg = colors.yellow, bold = true },
    Directory = { fg = colors.niagara },

    -- Selections and Search
    Visual = { bg = colors.selection },
    VisualNOS = { bg = colors.selection },
    Search = { fg = colors.bg, bg = colors.yellow },
    IncSearch = { fg = colors.bg, bg = colors.yellow },
    CurSearch = { fg = colors.bg, bg = colors.yellow },

    -- Popup Menu
    Pmenu = { fg = colors.fg, bg = colors.bg_lighter },
    PmenuSel = { fg = colors.bg, bg = colors.niagara },
    PmenuSbar = { bg = colors.bg_highlight },
    PmenuThumb = { bg = colors.niagara },

    -- Syntax highlighting
    Comment = { fg = colors.brown, italic = opts.italic_comments ~= false },
    Constant = { fg = colors.quartz },
    String = { fg = colors.green },
    Character = { fg = colors.green },
    Number = { fg = colors.fg },
    Boolean = { fg = colors.yellow },
    Float = { fg = colors.fg },

    Identifier = { fg = colors.fg },
    Function = { fg = colors.niagara },

    Statement = { fg = colors.yellow },
    Conditional = { fg = colors.yellow },
    Repeat = { fg = colors.yellow },
    Label = { fg = colors.yellow },
    Operator = { fg = colors.fg },
    Keyword = { fg = colors.yellow },
    Exception = { fg = colors.red },

    PreProc = { fg = colors.quartz },
    Include = { fg = colors.quartz },
    Define = { fg = colors.quartz },
    Macro = { fg = colors.quartz },
    PreCondit = { fg = colors.quartz },

    Type = { fg = colors.quartz },
    StorageClass = { fg = colors.yellow },
    Structure = { fg = colors.quartz },
    Typedef = { fg = colors.quartz },

    Special = { fg = colors.yellow },
    SpecialChar = { fg = colors.yellow },
    Tag = { fg = colors.niagara },
    Delimiter = { fg = colors.fg },
    SpecialComment = { fg = colors.wisteria },
    Debug = { fg = colors.red },

    Underlined = { fg = colors.niagara, underline = true },
    Ignore = { fg = colors.comment },
    Error = { fg = colors.error },
    Todo = { fg = colors.bg, bg = colors.yellow, bold = true },

    -- Treesitter
    ["@variable"] = { fg = colors.fg },
    ["@variable.builtin"] = { fg = colors.niagara },
    ["@variable.parameter"] = { fg = colors.fg },
    ["@variable.member"] = { fg = colors.fg },

    ["@constant"] = { fg = colors.quartz },
    ["@constant.builtin"] = { fg = colors.quartz },
    ["@constant.macro"] = { fg = colors.quartz },

    ["@string"] = { fg = colors.green },
    ["@string.escape"] = { fg = colors.yellow },
    ["@string.regexp"] = { fg = colors.wisteria },

    ["@character"] = { fg = colors.green },
    ["@number"] = { fg = colors.fg },
    ["@boolean"] = { fg = colors.yellow },
    ["@float"] = { fg = colors.fg },

    ["@function"] = { fg = colors.niagara },
    ["@function.builtin"] = { fg = colors.niagara },
    ["@function.macro"] = { fg = colors.quartz },
    ["@function.method"] = { fg = colors.niagara },

    ["@constructor"] = { fg = colors.niagara },
    ["@operator"] = { fg = colors.fg },
    ["@keyword"] = { fg = colors.yellow },
    ["@keyword.function"] = { fg = colors.yellow },
    ["@keyword.operator"] = { fg = colors.yellow },
    ["@keyword.return"] = { fg = colors.yellow },

    ["@conditional"] = { fg = colors.yellow },
    ["@repeat"] = { fg = colors.yellow },
    ["@exception"] = { fg = colors.red },

    ["@type"] = { fg = colors.quartz },
    ["@type.builtin"] = { fg = colors.quartz },
    ["@type.qualifier"] = { fg = colors.yellow },

    ["@property"] = { fg = colors.fg },
    ["@attribute"] = { fg = colors.quartz },
    ["@namespace"] = { fg = colors.quartz },

    ["@comment"] = { fg = colors.brown, italic = opts.italic_comments ~= false },
    ["@comment.todo"] = { fg = colors.bg, bg = colors.yellow, bold = true },
    ["@comment.warning"] = { fg = colors.bg, bg = colors.warning, bold = true },
    ["@comment.error"] = { fg = colors.bg, bg = colors.error, bold = true },
    ["@comment.note"] = { fg = colors.bg, bg = colors.info, bold = true },

    ["@tag"] = { fg = colors.niagara },
    ["@tag.attribute"] = { fg = colors.quartz },
    ["@tag.delimiter"] = { fg = colors.fg },

    ["@punctuation.delimiter"] = { fg = colors.fg },
    ["@punctuation.bracket"] = { fg = colors.fg },
    ["@punctuation.special"] = { fg = colors.yellow },

    ["@markup.heading"] = { fg = colors.yellow, bold = true },
    ["@markup.link"] = { fg = colors.niagara },
    ["@markup.link.url"] = { fg = colors.niagara, underline = true },
    ["@markup.raw"] = { fg = colors.green },
    ["@markup.strong"] = { bold = true },
    ["@markup.italic"] = { italic = true },

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
    TelescopePromptBorder = { fg = colors.niagara },
    TelescopePromptTitle = { fg = colors.yellow, bold = true },
    TelescopeSelection = { bg = colors.bg_highlight },
    TelescopeSelectionCaret = { fg = colors.yellow },

    -- Neo-tree
    NeoTreeNormal = { fg = colors.fg, bg = opts.transparent and "NONE" or colors.bg },
    NeoTreeNormalNC = { fg = colors.fg, bg = opts.transparent and "NONE" or colors.bg },
    NeoTreeDirectoryIcon = { fg = colors.niagara },
    NeoTreeDirectoryName = { fg = colors.niagara },
    NeoTreeGitAdded = { fg = colors.success },
    NeoTreeGitModified = { fg = colors.warning },
    NeoTreeGitDeleted = { fg = colors.error },
    NeoTreeGitUntracked = { fg = colors.green },

    -- WhichKey
    WhichKey = { fg = colors.yellow },
    WhichKeyGroup = { fg = colors.niagara },
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

    -- Lazy
    LazyButton = { fg = colors.fg, bg = colors.bg_lighter },
    LazyButtonActive = { fg = colors.bg, bg = colors.niagara },
    LazyH1 = { fg = colors.bg, bg = colors.yellow, bold = true },

    -- Mason
    MasonHeader = { fg = colors.bg, bg = colors.yellow, bold = true },
    MasonHighlight = { fg = colors.niagara },
    MasonHighlightBlockBold = { fg = colors.bg, bg = colors.niagara, bold = true },

    -- Cmp
    CmpItemAbbr = { fg = colors.fg },
    CmpItemAbbrMatch = { fg = colors.niagara, bold = true },
    CmpItemAbbrMatchFuzzy = { fg = colors.niagara },
    CmpItemKind = { fg = colors.wisteria },
    CmpItemMenu = { fg = colors.comment },
  }

  -- Apply all highlights
  for group, hl_opts in pairs(highlights) do
    vim.api.nvim_set_hl(0, group, hl_opts)
  end
end

return M
