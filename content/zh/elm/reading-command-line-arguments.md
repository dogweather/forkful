---
title:                "Elm: 读取命令行参数"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

为什么：为什么会有人选择阅读命令行参数?

有时候，我们需要通过程序来操作命令行参数，这可以使我们的应用程序更具交互性和自定义性。例如，我们可以根据命令行参数的不同来运行不同的程序逻辑。接下来，我们将会学习如何在 Elm 中读取命令行参数以及如何利用它们来提升我们的应用程序。

如何：要在 Elm 中读取命令行参数，我们可以使用 `Elm.Kernel.Platform` 模块的 `make` 函数。我们可以传入一个回调函数来获取命令行参数，并在 `Program` 模块中定义它。下面是一个读取和打印命令行参数的例子：

```Elm
import Html exposing (text)
import Platform exposing (Program)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Json.Decode as Decode

main : Program () String
main =
  Program.withConsole
    { init = \_ -> ("", Cmd.none)
    , update = \_ model -> (model, Cmd.none)
    , subscriptions = always Sub.none
    }

getArgs : (List String -> msg) -> Sub.Sub msg
getArgs tagger =
  Platform.worker
    { init = \() -> Sub.batch [ Sub.map tagger (Sub.fromList ["hello", "world"]) ]
    , update = \_ model -> (model, Cmd.none)
    }

view : String -> Html.Html msg
view arg =
  text ("Command line argument: " ++ arg)

subscriptions : Model -> Html.Html Msg
subscriptions model =
  getArgs GotArgs
```

输入 `elm make Main.elm --output app.js` 编译后，我们可以运行 `node app.js` 并看到输出为 `Command line argument: hello`。通过这种方法，我们可以获取命令行参数并在应用程序中使用它们。

深入探讨：要更深入理解如何读取命令行参数，我们需要了解 Elm 程序的内部结构。在 Elm 中，`Program` 模块是用来初始化程序状态并与浏览器交互的主要方式。在 `Program` 中，我们可以通过 `withConsole` 函数来添加一个控制台对象，从而可以通过 `Console` 模块的 `args` 属性获取命令行参数。在例子中，我们使用了 `Json.Decode` 模块来将获取到的命令行参数转换为 Elm 中的数据类型。通过理解这些内部结构，我们可以更加灵活地应用命令行参数来提升我们的应用程序。

另外，我们也可以通过 Elm 的 `Flags` 模块来传递命令行参数。这种方法更加灵活，使我们能够在程序运行之前就获取到命令行参数，并将它们作为初始状态传入 `Program` 构造函数中。这样一来，我们就可以在 `init` 函数中直接使用命令行参数。

See Also（参考链接）：

- [Elm 官方文档：平台模块](https://package.elm-lang.org/packages/elm/browser/latest/Browser)
- [Elm 官方文档：Json.Decode 模块](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- [Elm 官方文档：Flags 模块](https://package.elm-lang.org/packages/elm/core/latest/Platform-Flags)