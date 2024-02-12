---
title:                "开始一个新项目"
date:                  2024-01-20T18:03:43.701526-07:00
model:                 gpt-4-1106-preview
simple_title:         "开始一个新项目"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？
开始新项目，即初始化新程序结构。程序员做这个通常有两个原因：测试新思路，或开发具体应用。

## How to: 如何操作
```Haskell
-- 安装Stack工具
$ curl -sSL https://get.haskellstack.org/ | sh

-- 新建项目
$ stack new myproject

-- 切换到项目文件夹
$ cd myproject

-- 构建项目
$ stack build

-- 运行项目
$ stack exec myproject-exe
```

采用这些命令后，项目应建立并运行，显示相应的输出。

## Deep Dive 深入探讨
Haskell的项目管理常使用Stack工具，它在2014年出现，使项目设置和包管理更简单。Cabal是另一选项，但Stack提供更一致的构建经验。初始项目时，文件配置应详尽，可定义依赖和项目参数。Stack使用Cabal文件但添加了自己的层，便于管理多个项目和确保构建的一致性。

## See Also 参见链接
- Stack 官方网站: [https://docs.haskellstack.org](https://docs.haskellstack.org)
- Haskell项目构建交流论坛: [/r/haskell](https://www.reddit.com/r/haskell/)
- “Learn You a Haskell for Great Good!” 入门指南: [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
