---
date: 2024-01-20 17:44:10.659167-07:00
description: "How to: (\u3084\u308A\u65B9) Elm\u3067\u306FHTTP\u30D1\u30C3\u30B1\u30FC\
  \u30B8\u3092\u4F7F\u7528\u3057\u3066\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\
  \u30A6\u30F3\u30ED\u30FC\u30C9\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u7C21\u5358\
  \u306A\u4F8B\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.005907-06:00'
model: gpt-4-1106-preview
summary: "Elm\u3067\u306FHTTP\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3057\
  \u3066\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\
  \u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u7C21\u5358\u306A\u4F8B\u3067\u3059."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## How to: (やり方)
ElmではHTTPパッケージを使用してウェブページをダウンロードします。以下は簡単な例です。

```Elm
module Main exposing (..)

import Browser
import Html exposing (text)
import Http

type alias Model =
    { content : String }

type Msg
    = Fetch
    | FetchComplete String

init : () -> (Model, Cmd Msg)
init _ =
    ({ content = "" }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Fetch ->
            ( model
            , Http.get
                { url = "https://example.com"
                , expect = Http.expectString FetchComplete
                }
            )

        FetchComplete content ->
            ({ model | content = content }, Cmd.none)

view : Model -> Html.Html Msg
view model =
    text model.content

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
```

実行すると、`"https://example.com"`からのコンテンツが画面に表示されます。

## Deep Dive (深掘り)
Elmが登場したのは2012年で、その純粋関数的なアプローチはウェブ開発に新風をもたらしました。ダウンロードは`Http`パッケージを通じて行い、予想されるレスポンスタイプ(`Http.expectString`)に応じて処理を設定します。`Http.get` 関数で非同期リクエストを作成し、結果はメッセージとしてUpdate関数に渡されます。Elmはその他の手法に比べて副作用がなく、安全にデータを扱えることが特徴です。

## See Also (関連情報)
- Elm公式ドキュメント: [https://elm-lang.org/docs](https://elm-lang.org/docs)
- Elm HTTPパッケージ: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
