---
title:                "Fish Shell: 开始一个新项目"
programming_language: "Fish Shell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

##为什么

如果你想要提高你的命令行工作效率，Fish Shell 是一个值得尝试的强大的工具。它有简洁的语法和智能补全功能，可以帮助你更轻松地完成命令，节省宝贵的时间。所以，如果你是一位程序员或者经常使用命令行的用户，开展一个新的 Fish Shell 项目可以让你的工作更加高效快捷。

##如何进行

首先，你需要安装 Fish Shell。在终端中输入以下命令：

```Fish Shell
brew install fish
```

接下来，你需要设置 Fish Shell 为默认的 Shell。输入以下命令并按照提示操作：

```Fish Shell
echo "fish" >> /etc/shells
chsh -s /usr/local/bin/fish
```

现在，你可以打开一个新的终端窗口，就可以使用 Fish Shell 了。你可以尝试使用 Tab 键来补全命令，这是 Fish Shell 的智能补全功能之一，可以大大提高你的工作效率。

如果你想要定制 Fish Shell，你可以在 ~/.config/fish/config.fish 文件中写入自定义的命令。比如，如果你想要添加一个快捷命令来打开某个特定的文件夹，你可以这样写：

```Fish Shell
abbr -a myfolder "cd ~/Documents/MyFolder"
```

当你在终端中输入 myfolder 并按下 Tab 键时，Fish Shell 会自动将你导航到该文件夹。

##深入探索

如果你想要更深入地了解 Fish Shell，你可以查阅官方文档或者阅读其他的学习资源。Fish Shell 提供了丰富的内置函数和变量，可以让你编写更加强大的脚本。它也支持使用插件来扩展功能，比如 oh-my-fish 就是一个流行的插件管理器。

除了官方文档，你也可以参考这些链接来学习更多关于 Fish Shell 的知识：

- [Fish Shell 官方文档](https://fishshell.com/docs/current/)
- [Oh My Fish 官方网站](https://ohmyfish.gelbpunkt.net/)
- [Fish Shell GitHub 仓库](https://github.com/fish-shell/fish-shell)

##另请参阅

如果你对命令行工具感兴趣，你可能也会对这些相关主题感兴趣：

- [使用 Vim 编辑器提高工作效率](https://www.example.com/vim-tutorial)
- [学习使用 Homebrew 管理软件包](https://www.example.com/homebrew-tutorial)

谢谢阅读，希望这篇文章能够帮助你更好地了解和使用 Fish Shell。加油！