---
title:                "开始一个新项目"
html_title:           "Elm: 开始一个新项目"
simple_title:         "开始一个新项目"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 做什么和为什么？
创建一个新项目是指在编程中开始一个新的任务。程序员通常会创建新项目来实践新的编程技能或者解决一个特定的问题。

# 如何：
```Elm
module Main exposing (..)

import Html exposing (text)


main =
    text "Hello, world!"
```

输出：Hello, world!

```Elm
-- 在 REPL 中输入以下命令来调试代码

main
```

输出：Hello, world!

# 深入探讨：
- 历史背景：在 Elm 起源于 2012 年，是一种函数式编程语言，目的是构建可靠和高效的用户界面。它受到 Haskell 和 ML 等语言的影响。
- 其他选择：除了 Elm，其他的选择包括 React、Angular、Vue 等，它们也都是构建用户界面的流行方式。
- 实现细节：Elm 使用一种名为 Virtual DOM 的技术来提高性能和可靠性，它也支持自动内存管理。

# 参考链接：
- Elm 官方网站：https://elm-lang.org/
- Elm 文档：https://guide.elm-lang.org/
- Elm REPL：https://elm-lang.org/docs/repl