---
aliases:
- /zh/haskell/starting-a-new-project/
date: 2024-01-20 18:03:43.701526-07:00
description: "\u5F00\u59CB\u65B0\u9879\u76EE\uFF0C\u5373\u521D\u59CB\u5316\u65B0\u7A0B\
  \u5E8F\u7ED3\u6784\u3002\u7A0B\u5E8F\u5458\u505A\u8FD9\u4E2A\u901A\u5E38\u6709\u4E24\
  \u4E2A\u539F\u56E0\uFF1A\u6D4B\u8BD5\u65B0\u601D\u8DEF\uFF0C\u6216\u5F00\u53D1\u5177\
  \u4F53\u5E94\u7528\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.170590
model: gpt-4-1106-preview
summary: "\u5F00\u59CB\u65B0\u9879\u76EE\uFF0C\u5373\u521D\u59CB\u5316\u65B0\u7A0B\
  \u5E8F\u7ED3\u6784\u3002\u7A0B\u5E8F\u5458\u505A\u8FD9\u4E2A\u901A\u5E38\u6709\u4E24\
  \u4E2A\u539F\u56E0\uFF1A\u6D4B\u8BD5\u65B0\u601D\u8DEF\uFF0C\u6216\u5F00\u53D1\u5177\
  \u4F53\u5E94\u7528\u3002"
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
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
