---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

创建新项目就是从头开始编写一个全新的程序。程序员会这样做，因为他们需要创造出符合特定需求的软件，而这项需求之前没有任何现成的解决方案。

## 如何：

在 Elm 中，您可以使用以下命令来创建一个新项目：

```Elm
elm init
```

这个命令将创建一个新的`elm.json`文件，并且会初始化一个空的`src`文件夹。

## 深度了解

1. 历史背景: Elm 于 2012 年由社区开发出来，专为简化前端开发而生。它使用纯函数和静态类型，让你的项目从一开始就可以保持可维护性。

2. 替代方案: 如不使用 Elm，JavaScript、TypeScript, 和 Rust 等许多其他编程语言也同样能够创建新项目。选择哪种语言创建项目，主要取决于具体的业务需求和团队技能。

3. 实施细节: `elm init` 命令在项目根目录下创建 `elm.json` 文件，用于储存项目的配置信息，例如依赖项等。此外，它还创建一个 `src` 文件夹，这是你的代码存放的地方。

## 另请参见

- [Elm 官方文档](https://guide.elm-lang.org/)
- [Elm Github 仓库](https://github.com/elm)