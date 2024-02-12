---
title:                "开始一个新项目"
aliases:
- /zh/bash/starting-a-new-project.md
date:                  2024-01-20T18:03:00.703224-07:00
model:                 gpt-4-1106-preview
simple_title:         "开始一个新项目"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (什么与为什么？)
开始一个新项目就是创建一个全新的空间来编写代码和组织文件。程序员这么做是为了将想法变成现实，解决问题，或者学习新技术。

## How to: (如何开始：)
```Bash
# 创建一个新目录来存放项目
mkdir my_new_project
cd my_new_project

# 初始化版本控制
git init

# 创建一个README文件
echo "# My New Project" > README.md

# 查看项目结构
tree .

# 输出结果：
.
├── README.md
```

## Deep Dive (深入了解)
在Unix-like系统中，很早就使用目录来组织文件。Bash作为一个命令行界面，让我们可以用命令来管理这些目录和文件。版本控制，像Git，是后来出现的，用来跟踪代码变化。不只是Git，还有SVN或Mercurial等，但Git现在最流行。在项目开始时就使用Git可以帮助你更好地管理变更历史。

创建`README`文件是一个好习惯。这就像给你的项目设定了一个入口，同时也是给别人的第一印象。README中通常包括项目介绍、如何使用、如何贡献等等。

使用 `tree` 命令可以帮助你快速可视化目录结构，但这个命令可能需要你单独安装。

## See Also (另请参阅)
- [Git](https://git-scm.com/)
- [Markdown Guide](https://www.markdownguide.org/)
- [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial)
