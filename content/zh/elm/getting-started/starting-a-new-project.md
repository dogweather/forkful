---
date: 2024-01-20 18:03:33.235907-07:00
description: "\u5F00\u59CB\u65B0\u9879\u76EE\u610F\u5473\u7740\u521B\u5EFA\u4E00\u5957\
  \u5168\u65B0\u7684\u4EE3\u7801\u5E93\u6765\u5F00\u53D1\u8F6F\u4EF6\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u89E3\u51B3\u65B0\u95EE\u9898\uFF0C\u63A2\
  \u7D22\u521B\u610F\u6216\u5B9E\u73B0\u5177\u4F53\u529F\u80FD\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.450149-06:00'
model: gpt-4-1106-preview
summary: "\u5F00\u59CB\u65B0\u9879\u76EE\u610F\u5473\u7740\u521B\u5EFA\u4E00\u5957\
  \u5168\u65B0\u7684\u4EE3\u7801\u5E93\u6765\u5F00\u53D1\u8F6F\u4EF6\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u89E3\u51B3\u65B0\u95EE\u9898\uFF0C\u63A2\
  \u7D22\u521B\u610F\u6216\u5B9E\u73B0\u5177\u4F53\u529F\u80FD\u3002"
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
开始新项目意味着创建一套全新的代码库来开发软件。程序员这么做是为了解决新问题，探索创意或实现具体功能。

## How to: (如何操作：)
在Elm中开始新项目很简单。打开你的终端（Terminal）或命令提示符（CMD），运行以下命令：

```Elm
elm init
```

这会建立一个新的`elm.json`文件并创建项目文件夹结构。简易的Hello World示例如下：

```Elm
module Main exposing (..)
import Html exposing (text)

main =
    text "你好，世界！"
```

保存为`Main.elm`文件并运行：

```Elm
elm reactor
```

浏览器访问`http://localhost:8000`会显示你的Hello World。

## Deep Dive (深入探究)
在历史上，Elm是为了让前端开发更加可靠而创建的，它提供无运行错误的保证。相比其他JavaScript框架，Elm用严格的类型系统和不可变性原则提供了一种不同的开发体验。

Elm的项目结构通常包括`elm.json`配置文件和一个源代码目录。`elm.json`里配置了项目依赖和Elm版本。源代码目录是存放`.elm`文件的地方。这些文件应该遵循明确的模块系统，这也有益于代码的维护和团队合作。

其他开始Elm项目的方法包括使用第三方CLI工具，例如`create-elm-app`，但直接使用`elm`命令是最基本也最直接的。

## See Also (参见)
- Elm官方文档：[https://guide.elm-lang.org/](https://guide.elm-lang.org/)
- Elm语言包：[https://package.elm-lang.org/](https://package.elm-lang.org/)
- 创建Elm应用：[https://github.com/halfzebra/create-elm-app](https://github.com/halfzebra/create-elm-app)
