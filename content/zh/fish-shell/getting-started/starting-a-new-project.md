---
date: 2024-01-20 18:03:30.176225-07:00
description: "\u5F00\u65B0\u9879\u76EE\uFF0C\u5C31\u662F\u521B\u5EFA\u4ECE\u96F6\u5F00\
  \u59CB\u7684\u4EE3\u7801\u96C6\u5408\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\
  \u4E3A\u4E86\u89E3\u51B3\u65B0\u95EE\u9898\uFF0C\u5F00\u53D1\u65B0\u529F\u80FD\uFF0C\
  \u6216\u8005\u5C1D\u8BD5\u65B0\u60F3\u6CD5\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.266267-06:00'
model: gpt-4-1106-preview
summary: "\u5F00\u65B0\u9879\u76EE\uFF0C\u5C31\u662F\u521B\u5EFA\u4ECE\u96F6\u5F00\
  \u59CB\u7684\u4EE3\u7801\u96C6\u5408\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\
  \u4E3A\u4E86\u89E3\u51B3\u65B0\u95EE\u9898\uFF0C\u5F00\u53D1\u65B0\u529F\u80FD\uFF0C\
  \u6216\u8005\u5C1D\u8BD5\u65B0\u60F3\u6CD5\u3002"
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
weight: 1
---

## What & Why? (是什么 & 为什么?)
开新项目，就是创建从零开始的代码集合。程序员这么做是为了解决新问题，开发新功能，或者尝试新想法。

## How to: (如何操作：)
在Fish Shell中启动新项目，基本就是建立新目录，初始化版本控制，和创建初始文件。
```Fish Shell
# 新建项目目录
mkdir my_project
cd my_project

# 初始化git（如果使用）
git init

# 创建基础文件
touch README.md main.fish .gitignore
```
输出没有特殊显示，命令执行后，新的项目结构被创建。

## Deep Dive (深入了解)
从历史上看，项目开始通常手动进行。进入现代，脚手架工具自动化这个过程，比如 `yeoman` 或 `cargo`。Fish Shell本身是个现代化的Shell，强调易用性和可发现性。其他Shell（比如Bash或Zsh）也有自己的方法启动项目，但Fish提供了独特的便利特性，比如丰富的提示和自动建议。

实现层面，新项目通常包括目录结构，基础代码，配置文件，和版本控制。Fish Shell不直接提供项目管理工具，但它与git等工具无缝协作，让项目开始变得十分流畅。使用像 `fisher` 或 `oh-my-fish` 之类的包管理器，可以进一步扩展Fish的功能，提升项目开发效率。

## See Also (另请参阅)
- [Fish Shell官网](https://fishshell.com)
- [Git版本控制系统](https://git-scm.com/)
- [Fisher包管理器](https://github.com/jorgebucaran/fisher)
- [Oh My Fish包管理器](https://github.com/oh-my-fish/oh-my-fish)
