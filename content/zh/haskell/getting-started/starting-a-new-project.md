---
date: 2024-01-20 18:03:43.701526-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C \u91C7\u7528\u8FD9\u4E9B\u547D\u4EE4\
  \u540E\uFF0C\u9879\u76EE\u5E94\u5EFA\u7ACB\u5E76\u8FD0\u884C\uFF0C\u663E\u793A\u76F8\
  \u5E94\u7684\u8F93\u51FA\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.128446-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C \u91C7\u7528\u8FD9\u4E9B\u547D\u4EE4\u540E\uFF0C\
  \u9879\u76EE\u5E94\u5EFA\u7ACB\u5E76\u8FD0\u884C\uFF0C\u663E\u793A\u76F8\u5E94\u7684\
  \u8F93\u51FA\u3002"
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
weight: 1
---

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
