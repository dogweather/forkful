---
title:    "Elm: 读取命令行参数"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

为什么要学习命令行参数？命令行参数是一个非常有用的工具，可以让我们在运行程序时输入一些信息，这样程序就能根据我们输入的不同参数，展现不同的功能或数据。

## 怎么做

```Elm
import Platform exposing (Program)
import Html exposing (text)
import Browser
import Browser.Navigation as Nav

type Msg
    = ReadArgs (Maybe String)


main : Program () String Msg
main =
    Browser.element
        { init = \_ -> ( Nothing, Cmd.none )
        , view = \_ -> text "Enter a command line argument."
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update msg args =
    case msg of
        ReadArgs maybeArg ->
            ( Nothing, Cmd.none )


onUrlRequest : Nav.UrlRequest -> Msg
onUrlRequest request =
    ReadArgs <| Nav.parseHash request

```

在这个示例代码中，我们引入了 Elm 的 `Platform` 和 `Html` 模块，以及 `Browser` 和 `Browser.Navigation` 模块。我们定义了一个 `Msg` 类型，有一个 `ReadArgs` 构造函数，它接收一个 `Maybe String` 类型的参数。接着在 `main` 函数中，我们使用 `Browser.element` 函数来创建一个程序。我们通过 `update` 函数来更新我们程序的状态，并通过 `onUrlRequest` 函数来读取我们输入的命令行参数。最后，我们通过 `text` 函数来展示用户输入的参数。

## 深入学习

命令行参数可以帮助我们轻松地处理程序的输入信息，同时也能让我们在开发调试过程中更加灵活。除了上面的示例代码，我们还可以使用 `Program` 和 `Html` 模块中的其他函数来处理输入的参数，比如 `Browser.view`，`Browser.element`，`Browser.document`等等。此外，我们也可以通过 `Browser.Navigation` 模块来获取更多关于用户输入信息的细节。

## 参考链接

- Elm 官方文档：https://guide.elm-lang.org/
- Elm 中文社区：https://elm-china.org/
- Elm 包管理器：https://package.elm-lang.org/