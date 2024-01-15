---
title:                "开始新项目"
html_title:           "Fish Shell: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 为什么

在编程世界中，Fish Shell 是一个备受赞誉的轻量级、快速、可配置的命令行壳程序，它的设计灵感来自于 Bash 和 Zsh。如果你想要提高你的命令行操作效率，或者想要探索一种新的、简洁的编程方式，那么开始学习Fish Shell 绝对是一个明智的选择。

## 如何开始

Fish Shell 在 Linux、macOS 和 Windows 的 WSL 环境中都可以使用。如果你是通过 Homebrew 安装的，那么只需在终端中输入 `fish`，就可以启动 Fish Shell。如果你是在其他操作系统中使用，请自行安装或参考官方文档。

```Fish Shell
$ fish
```

接下来，我们来看一些常用的 Fish Shell 命令和用法：

1. 察看当前环境变量：在 Fish Shell 中，可以通过 `echo $PATH` 来查看当前的环境变量，和其他壳程序类似。如果你想要添加新的环境变量，可以在 `~/.config/fish/config.fish` 文件中添加新的内容，并使用命令 `set -U` 来设置新的环境变量。

2. 自动补全：Fish Shell 支持自动补全功能，无论是文件名、路径，还是命令，都可以通过按下 `Tab` 键来快速补全，大大提高了操作效率。

3. 命令历史：如果你是一个爱好者，经常需要使用命令历史来查找特定的命令，那么 Fish Shell 也许适合你。在 Fish Shell 中，可以通过 `history` 命令来查看最近使用过的命令，并可以使用 `ctrl+r` 来快速搜索过去使用过的命令。

以上只是 Fish Shell 中一小部分常用的命令和用法，如果想要了解更多，建议参考官方文档或搜索相关资源。

## 深入学习

如果你想要更深入地学习并使用 Fish Shell，这里有一些 tips 可以帮助你：

1. 利用别名：在 Fish Shell 中，可以通过 `alias` 命令来创建别名。比如，你可以将 `ls -lh` 创建为 `llh` 的别名，这样每次使用 `llh` 就相当于执行了 `ls -lh` 命令。

2. Fish Shell 的命令补全系统非常强大，可以通过修改 `~/.config/fish/completions/` 目录中的脚本来定义自己的命令补全。

3. 如果你发现某些命令无法在 Fish Shell 中正常运行，可以尝试使用 `fish_update_completions` 命令来更新 Fish Shell 的命令补全系统。

4. Fish Shell 的配置文件位于 `~/.config/fish/config.fish`，可以通过修改此文件来自定义 Fish Shell 的行为和外观。

总的来说，Fish Shell 在脚本编写、命令操作、自动补全等方面都有很多有趣和实用的功能，值得你花一些时间来学习和使用。

## 参考链接

- [Fish Shell 官方文档](https://fishshell.com/docs/current/index.html)
- [官方 GitHub 仓库](https://github.com/fish-shell/fish-shell)
- [Awesome Fish Shell：收集了大量有用的 Fish Shell 资源](https://github.com/jorgebucaran/awesome-fish)