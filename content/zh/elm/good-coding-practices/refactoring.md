---
date: 2024-01-26 01:17:54.689934-07:00
description: "\u91CD\u6784\u672C\u8D28\u4E0A\u5C31\u662F\u5BF9\u4F60\u7684\u4EE3\u7801\
  \u5E93\u8FDB\u884C\u201C\u6625\u5B63\u5927\u626B\u9664\u201D\u2014\u2014\u8FD9\u610F\
  \u5473\u7740\u5728\u4E0D\u6539\u53D8\u4EE3\u7801\u5916\u90E8\u884C\u4E3A\u7684\u524D\
  \u63D0\u4E0B\uFF0C\u91CD\u65B0\u7EC4\u7EC7\u73B0\u6709\u4EE3\u7801\u3002\u7A0B\u5E8F\
  \u5458\u4E4B\u6240\u4EE5\u8FD9\u4E48\u505A\uFF0C\u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\
  \u66F4\u6613\u8BFB\u3001\u964D\u4F4E\u590D\u6742\u6027\u3001\u63D0\u9AD8\u53EF\u7EF4\
  \u62A4\u6027\uFF0C\u4EE5\u53CA\u4F7F\u5176\u66F4\u6613\u4E8E\u6269\u5C55\u3002"
lastmod: '2024-03-13T22:44:47.682636-06:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u672C\u8D28\u4E0A\u5C31\u662F\u5BF9\u4F60\u7684\u4EE3\u7801\
  \u5E93\u8FDB\u884C\u201C\u6625\u5B63\u5927\u626B\u9664\u201D\u2014\u2014\u8FD9\u610F\
  \u5473\u7740\u5728\u4E0D\u6539\u53D8\u4EE3\u7801\u5916\u90E8\u884C\u4E3A\u7684\u524D\
  \u63D0\u4E0B\uFF0C\u91CD\u65B0\u7EC4\u7EC7\u73B0\u6709\u4EE3\u7801\u3002\u7A0B\u5E8F\
  \u5458\u4E4B\u6240\u4EE5\u8FD9\u4E48\u505A\uFF0C\u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\
  \u66F4\u6613\u8BFB\u3001\u964D\u4F4E\u590D\u6742\u6027\u3001\u63D0\u9AD8\u53EF\u7EF4\
  \u62A4\u6027\uFF0C\u4EE5\u53CA\u4F7F\u5176\u66F4\u6613\u4E8E\u6269\u5C55\u3002."
title: "\u4EE3\u7801\u91CD\u6784"
weight: 19
---

## 何为重构？以及为什么重构？

重构本质上就是对你的代码库进行“春季大扫除”——这意味着在不改变代码外部行为的前提下，重新组织现有代码。程序员之所以这么做，是为了使代码更易读、降低复杂性、提高可维护性，以及使其更易于扩展。

## 如何进行重构：

假设你有一个Elm函数功能过于复杂，比如UI逻辑和状态更新混为一谈。这是重构的完美候选。原始代码如下：

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

重构之后，我们通过将逻辑拆分到不同的函数中来分离关注点：

```Elm
-- 更新逻辑是分开的
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- 格式化（视图）逻辑也是分开的
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- 举例来说，如果输入太短就清除输入。

-- 更新函数现在使用辅助函数
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
经过这些改变，你有了清晰的分离，每个函数都更易于理解和测试。

## 深入探讨
把重构作为一种正式的实践可以追溯到编程的早期，当时代码变更的成本已经被认识到是开发过程中的一个关键方面。特别是，马丁·福勒在1990年代末出版的《重构：改善既有代码的设计》一书，真正为重构设立了基于结构化方法的舞台，并提供了用以识别重构机会的“代码异味”目录。

在Elm的语境中，重构利用了该语言的强项，比如其强类型系统，这在过程中促进了信心。手动重构的替代方法可以包括自动化代码转换工具，但与一些更老的语言相比，Elm在这一领域的工具仍在成熟中。实现细节通常围绕着常见的重构，如函数提取、重命名和简化条件表达式。Elm的编译器是重构的关键盟友，因为它不会让你轻易脱身——每当有什么地方不对劲时，它都会大声提醒，确保你重构的代码依然能够正常工作。

## 另见
- [《重构：改善既有代码的设计》作者马丁·福勒](https://martinfowler.com/books/refactoring.html)
- [Elm论坛 - 有关重构的话题](https://discourse.elm-lang.org/search?q=refactoring)
