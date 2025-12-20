-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")

vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 1
vim.g.netrw_winsize = 50
vim.g.netrw_browse_split = 4
vim.g.netrw_altv = 1
vim.g.netrw_keepdir = 0

-- Open netrw when starting nvim with no file
vim.api.nvim_create_autocmd("VimEnter", {
  nested = true,
  callback = function()
    if vim.fn.argc() == 0 then
      vim.cmd("Explore")
    end
  end,
})
