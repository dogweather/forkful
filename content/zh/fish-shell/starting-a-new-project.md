---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么和为什么？
开始一个新项目就是创建一个全新的编程工作据点，程序员这么做是为了实现特定的软件目标。

## 如何做：
在下面的```Fish Shell```代码块中，我们将看到如何启动一个新项目。

```Fish Shell
# 创建一个新的文件夹
mkdir newproject
cd newproject

# 初始化Git
git init

# 创建一个简单的README文件
echo "# New Project" > README.md
```
在上述操作之后，你的新项目目录将包含一个.git目录和README.md文件。

## 深入探索
开始一个新项目在过去的几十年间一直是编程的核心部分。虽然还有其他方式可以管理你的代码，例如子版本（SVN），和梅丽尓神墓（Mercurial），但Git已经成为最优先的工具，几乎所有现代开发者在起步时都会选择Git。此外，每个项目都应该有描述项目内容的Readme文件。

## 另请参阅：
1. [学习Git](https://git-scm.com/book/zh/v2)
2. [Fish Shell文档](https://fishshell.com/docs/current/tutorial.html)

备注：为了减少不必要的错误，建议总是利用你的编辑器的文件保存对话框来创建新文件或目录。