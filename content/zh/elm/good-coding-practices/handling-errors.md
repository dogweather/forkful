---
title:                "处理错误"
aliases: - /zh/elm/handling-errors.md
date:                  2024-01-26T00:51:35.419701-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/handling-errors.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
处理错误意味着编写能够预料并应对问题出现时的代码。程序员这样做是为了防止程序崩溃、保护数据完整性，以及为用户提供优雅的备选方案。

## 如何操作：
Elm 的核心哲学是没有运行时异常。因此，Elm 利用其类型系统以及像 `Maybe` 和 `Result` 这样的类型来处理错误。

`Maybe` 场景：

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- 当你运行它时：

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

`Result` 场景：

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- 使用它的时候：

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## 深入探究
Elm 的类型系统是严格的，有助于及早捕捉错误。历史上，大多数语言依赖异常和运行时检查，但 Elm 选择了编译时保证。像 `Result` 这样的替代方案能够提供详细的错误信息，而 `Maybe` 对于是-否场景则更加简单。Elm 的错误处理鼓励开发者事先考虑所有路径，避免忘记错误情况的陷阱。

## 参见：
- Elm 官方指南关于错误处理的章节：[错误处理 - 一个介绍](https://guide.elm-lang.org/error_handling/)
- Elm `Maybe` 文档：[Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Elm `Result` 文档：[Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
