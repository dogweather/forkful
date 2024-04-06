---
date: 2024-01-20 17:44:12.925690-07:00
description: "How to \u5982\u4F55\u505A \u5728Elm\u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\
  \u7528`Http.get`\u51FD\u6570\u4E0B\u8F7D\u7F51\u9875\u5185\u5BB9\u3002\u4EE5\u4E0B\
  \u662F\u4E00\u4E2A\u5982\u4F55\u83B7\u53D6\u7F51\u9875\u5185\u5BB9\u7684\u4F8B\u5B50\
  \uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.989157-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

## How to 如何做
在Elm中，你可以使用`Http.get`函数下载网页内容。以下是一个如何获取网页内容的例子：

```Elm
import Browser
import Http
import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)

type Msg
    = Fetch
    | ReceiveResponse (Result Http.Error String)

type alias Model =
    { content : String
    , error : Maybe String
    }

init : Model
init =
    { content = ""
    , error = Nothing
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Fetch ->
            ( model
            , Http.get
                { url = "https://example.com"
                , expect = Http.expectString ReceiveResponse
                }
            )
        ReceiveResponse (Ok body) ->
            ( { model | content = body, error = Nothing }, Cmd.none )

        ReceiveResponse (Err _) ->
            ( { model | error = Just "Failed to load content." }, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick Fetch ] [ text "Load Page Content" ] ]
        , div []
            [ case model.content of
                "" ->
                    case model.error of
                        Just err ->
                            text err

                        Nothing ->
                            text "Content will appear here after fetching."
                content ->
                    text content
            ]
        ]

main =
    Browser.sandbox { init = init, update = update, view = view }
```

运行以上代码，点击按钮会尝试下载并显示"example.com"网站的内容。

## Deep Dive 深入探索
Elm使用纯函数式的语言，这使得它在处理HTTP请求方面有些与众不同。过去，Elm提供了特定的模块比如`elm-lang/http`。现在，只需要简洁的`Http`模块就能处理大多数的HTTP请求。
除了`Http.get`，还有其他函数如`Http.post`、`Http.request`允许你以更复杂的方式与服务器交互。
实现时，下载的内容作为消息（在上面的例子中是`ReceiveResponse`）传回到应用程序的`update`函数，在这里处理结果。

## See Also 另请参阅
- Elm的官方HTTP包文档：[http://package.elm-lang.org/packages/elm/http/latest](http://package.elm-lang.org/packages/elm/http/latest)
- Elm指南中的HTTP部分：[https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)
- Elm的官方讨论社区：[https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
