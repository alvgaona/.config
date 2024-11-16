return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-neotest/neotest-python",
    },
    opts = function(_, opts)
      table.insert(
        opts.adapters,
        require("neotest-python")({
          dap = { justMyCode = false, console = "integratedTerminal" },
          args = { "--log-level", "DEBUG" },
          runner = "pytest",
          python = ".venv/bin/python",
        })
      )
    end,
  },
}
