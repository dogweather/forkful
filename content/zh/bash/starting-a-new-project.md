---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
启动新项目就是从零开始创建新的软件项目。程序员之所以要这样做，是因为他们能够根据自身的特定需求和目标来自定义功能。

## 如何：
使用 Bash 创建新的目录并初始化 Git 仓库。有两个主要步骤：

```Bash
mkdir new_project
cd new_project
git init
```

这是朴素的输出：

```Bash
Initialized empty Git repository in your_path/new_project/.git/
```

此脚本首先创建一个名为 'new_project' 的新目录，然后将其作为当前工作目录。最后，使用 'git init' 命令在新目录中初始化一个空的 Git 仓库。

## 深入了解
在计算机编程的早期，编程语言主要通过打孔卡进行编码。每一个新项目都需要从头开始，需要大量的时间和精力。将 Bash 脚本自动化这个过程，使我们能够立即开始编码。

替代方案包括其他Shell脚本语言，例如 Zsh 和 Fish，也可以做到同样的事。然而， Bash 是其中最受欢迎兼最被广泛使用的。这是因为 Bash 作为大多数 Linux 分发版的默认 shell，往往是开发者首选的脚本语言。

实现这一点的关键在于 'git init'，这个命令在当前目录下创建一个新的 Git 仓库。这使得其变成了一个可以使用 Git 在多台计算机之间同步，并追踪改动历史的工作区。

## 参考
- [创立新的 Git 仓库 | Pro Git (中文版)](https://git-scm.com/book/zh/v2/%E8%B5%B7%E6%AD%A5-%E5%88%9B%E5%BB%BA%E6%96%B0%E7%9A%84-git-%E4%BB%93%E5%BA%93)
- [Bash Beginner's Guide](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/index.html)

再见 (Goodbye)!