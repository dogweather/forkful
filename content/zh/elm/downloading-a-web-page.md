---
title:                "Elm: 下载网页。"
simple_title:         "下载网页。"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么

在当今的互联网世界，网页是我们获取信息和沟通的主要渠道。因此，学习如何下载网页是很重要的，它可以让我们更加高效地浏览和使用网页。而Elm语言提供了强大的工具来实现这一目标。

## 如何实现

实现下载网页需要以下三个步骤：

1. 创建一个HTTP请求，以指定要下载的网页的URL。
```Elm
import Http

requestPage : Http.Request
requestPage =
    Http.get "https://www.example.com/"
```

2. 发送请求并接收响应。
```Elm
import Html exposing (text)
import Http
import String

getPage : String -> Task Http.Error String
getPage url =
    Http.getString url

response : Task Http.Error String
response =
  getPage "https://www.example.com/"

page : Html.Html msg
page =
  Html.text "Loading..."

main =
  Html.program { init = (), view = page, subscriptions = ignore }

```

3. 处理响应结果，例如将其显示在页面上。
```Elm
import Html exposing (div, text)
import Html.Attributes exposing (style, class, width, height)
import String
import Task exposing (Task)
import Http

type alias Model = 
 { page : Maybe String
  }

type Msg
  = SetPage (Task Http.Error String)

init : ( Model, Cmd Msg )
init =
  ( Model Nothing, Http.get "https://www.example.com/" |> Task.perform SetPage )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
  case msg of
    SetPage task ->
      let
        getPage data =
          data |> case data of
            Ok data ->
              String.slice 0 10 data

            Err error ->
              "Error: " ++ Http.errorToString error
      in
        ( { mdl | page = Maybe.map getPage task }, Cmd.none )

view : Model -> Html.Html Msg
view mdl =
  div [ style [ ( "margin-top", "50px" ), ( "margin-left", "50px" ) ]] [ page mdl ]

main =
  Html.program { init = init, update = update, view = view, subscriptions = ignore }

```

## 深入了解

使用Elm语言下载网页的过程涉及到了许多有趣的概念，比如函数的纯函数式编程、类型推断和消息传递。对于想要更深入了解这些概念的读者，可以参考以下链接：

- [Elm官方网站](https://elm-lang.org/)
- [Elm编程语言入门指南（中文）](https://elm-lang.org/docs/getting-started)
- [Elm包管理器（中文）](https://elm-lang.org/docs/install)
- [Elm编程语言核心概念（中文）](https://elm-lang.org/docs/architecture)
- [Elm编程语言官方博客（英文）](https://elm-lang.org/blog)

## 参考链接

- [Elm编程语言官方文档（中文）](https://elm-lang.org/docs)
- [Elm编程语言官方论坛（英文）](https://discourse.elm-lang.org/)
- [Elm语言中心Github仓库（英文）](https://github.com/elm/elm-lang.org)
- [Elm包管理器（英文）](https://package.elm-lang.org/)
- [Elm编程语言案例网站（英文）](https://elm-lang.org/examples)