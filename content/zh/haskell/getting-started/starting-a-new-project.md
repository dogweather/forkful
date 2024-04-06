---
date: 2024-01-20 18:03:43.701526-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C Haskell\u7684\u9879\u76EE\u7BA1\u7406\
  \u5E38\u4F7F\u7528Stack\u5DE5\u5177\uFF0C\u5B83\u57282014\u5E74\u51FA\u73B0\uFF0C\
  \u4F7F\u9879\u76EE\u8BBE\u7F6E\u548C\u5305\u7BA1\u7406\u66F4\u7B80\u5355\u3002Cabal\u662F\
  \u53E6\u4E00\u9009\u9879\uFF0C\u4F46Stack\u63D0\u4F9B\u66F4\u4E00\u81F4\u7684\u6784\
  \u5EFA\u7ECF\u9A8C\u3002\u521D\u59CB\u9879\u76EE\u65F6\uFF0C\u6587\u4EF6\u914D\u7F6E\
  \u5E94\u8BE6\u5C3D\uFF0C\u53EF\u5B9A\u4E49\u4F9D\u8D56\u548C\u9879\u76EE\u53C2\u6570\
  \u3002Stack\u4F7F\u7528Cabal\u6587\u4EF6\u4F46\u6DFB\u52A0\u4E86\u81EA\u5DF1\u7684\
  \u5C42\uFF0C\u4FBF\u4E8E\u7BA1\u7406\u591A\u4E2A\u9879\u76EE\u548C\u786E\u4FDD\u6784\
  \u5EFA\u7684\u4E00\u81F4\u6027\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.026422-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C Haskell\u7684\u9879\u76EE\u7BA1\u7406\u5E38\u4F7F\
  \u7528Stack\u5DE5\u5177\uFF0C\u5B83\u57282014\u5E74\u51FA\u73B0\uFF0C\u4F7F\u9879\
  \u76EE\u8BBE\u7F6E\u548C\u5305\u7BA1\u7406\u66F4\u7B80\u5355\u3002Cabal\u662F\u53E6\
  \u4E00\u9009\u9879\uFF0C\u4F46Stack\u63D0\u4F9B\u66F4\u4E00\u81F4\u7684\u6784\u5EFA\
  \u7ECF\u9A8C\u3002\u521D\u59CB\u9879\u76EE\u65F6\uFF0C\u6587\u4EF6\u914D\u7F6E\u5E94\
  \u8BE6\u5C3D\uFF0C\u53EF\u5B9A\u4E49\u4F9D\u8D56\u548C\u9879\u76EE\u53C2\u6570\u3002\
  Stack\u4F7F\u7528Cabal\u6587\u4EF6\u4F46\u6DFB\u52A0\u4E86\u81EA\u5DF1\u7684\u5C42\
  \uFF0C\u4FBF\u4E8E\u7BA1\u7406\u591A\u4E2A\u9879\u76EE\u548C\u786E\u4FDD\u6784\u5EFA\
  \u7684\u4E00\u81F4\u6027\u3002"
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
