---
aliases:
- /ja/elm/downloading-a-web-page/
date: 2024-01-20 17:44:10.659167-07:00
description: "Web\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u306F\
  \u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306EHTML\u30B3\u30F3\u30C6\
  \u30F3\u30C4\u3092\u30ED\u30FC\u30AB\u30EB\u306B\u4FDD\u5B58\u3059\u308B\u3053\u3068\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u306E\u51E6\
  \u7406\u3084\u5206\u6790\u306E\u305F\u3081\u3001\u3042\u308B\u3044\u306F\u30AA\u30D5\
  \u30E9\u30A4\u30F3\u3067\u306E\u8AAD\u307F\u8FBC\u307F\u306E\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.839799
model: gpt-4-1106-preview
summary: "Web\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u306F\u3001\
  \u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306EHTML\u30B3\u30F3\u30C6\u30F3\
  \u30C4\u3092\u30ED\u30FC\u30AB\u30EB\u306B\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u306E\u51E6\u7406\
  \u3084\u5206\u6790\u306E\u305F\u3081\u3001\u3042\u308B\u3044\u306F\u30AA\u30D5\u30E9\
  \u30A4\u30F3\u3067\u306E\u8AAD\u307F\u8FBC\u307F\u306E\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
Webページのダウンロードは、インターネット上のHTMLコンテンツをローカルに保存すること。プログラマーは、データの処理や分析のため、あるいはオフラインでの読み込みのためにこれを行います。

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
