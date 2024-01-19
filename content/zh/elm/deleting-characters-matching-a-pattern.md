---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么和为什么？
删除匹配模式的字符是一种在给定字符串中删除与指定模式匹配的所有字符的过程。编程师之所以这样做，是因为它能够帮助我们去除不需要的数据，简化字符串操作。

## 如何做：
```Elm 
module Main exposing (..)
import String
main =
   let
       str = "Hello, Elm World!345"
       removeChars = [',', '!', '3']
       result = String.foldl (\c acc -> if (List.member c removeChars) then acc else c::acc) [] str |> List.reverse |> String.fromList
   in
   Html.text result
```
在这个例子中，我们创建了一个`str`变量来存储我们的字符串，`removeChars`列表用于存放我们要从字符串中删除的所有字符。我们使用了Elm的`foldl`函数，然后通过`List.member` 检查每个字符是否位于我们的`removeChars`列表中，如果是，我们就将其删除。

代码运行结果为：
```
Hello Elm World45
```
你看，我们已经成功删除了所有匹配的字符。

## 深入探讨：
删除匹配模式的字符在编程历史上一直是常见的需求。在80年代，C语言的程序员经常使用ANSI C的`strcspn()`函数来执行此操作。在崭新的Elm编程中, 我们使用高阶函数如 `foldl` 来实现这一功能。

你也可以使用正则表达式删除匹配的字符，不过在Elm中，并没有提供正则表达式的函数库。Elm语言倾向于使用纯函数编程模式, 享有更容易理解和预测的代码执行过程。

## 另请参见：
你可以在以下链接找到更多关于String包、List包、和`foldl`函数的相关情况：
- String包：https://package.elm-lang.org/packages/elm/core/latest/String
- List包：https://package.elm-lang.org/packages/elm/core/latest/List
- Foldl函数：https://package.elm-lang.org/packages/elm/core/latest/List#foldl