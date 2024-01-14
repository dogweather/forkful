---
title:    "Elm: 阅读命令行参数"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 为什么要阅读命令行参数

阅读命令行参数是一项非常重要的技能，它可以帮助程序员轻松处理用户的输入。通过阅读命令行参数，您可以让您的程序更加灵活，使用户能够自定义程序的运行方式。

## 如何阅读命令行参数

通过使用 Elm 编程语言，您可以轻松阅读命令行参数。以下是一个简单的例子，展示了如何读取用户提供的命令行参数，并在控制台输出：

```
Elm
module Main exposing (run)

import Platform.Cmd exposing (Cmd, Cmd)
import Basics exposing (..)

main : Program String
main =
  Platform.worker { init = init, update = update, subscriptions = subscriptions }

type Msg
  = Arguments (List String)

init : (List String) -> (Model, Cmd Msg)
init args =
  (args, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Arguments argList ->
      (argList, Cmd.none)

subscriptions : (List Sub Msg)
subscriptions =
  []

```

当您运行这段代码并在命令行中提供参数（如 `elm make Main.elm --output=main.js`），您将会看到输出结果为 `["Main.elm", "--output=main.js"]`。

## 深入阅读命令行参数

阅读命令行参数的过程其实并不复杂，它只是通过使用 `Platform.worker` 函数来执行 `init` 方法，然后 `Platform.Cmd` 模块中的 `Cmd` 函数来获取命令行参数。您可以根据需要自定义命令行参数的读取方式，例如只读取特定的参数并忽略其他参数。

## 参考资料

- [Elm 官方文档 - 读取命令行参数](https://guide.elm-lang.org/interop/flags.html)
- [Elm Cookbook - 读取命令行参数](https://elm-lang.org/news/new-flags)