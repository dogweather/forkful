---
title:                "ウェブページのダウンロード"
date:                  2024-01-20T17:44:10.659167-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/downloading-a-web-page.md"
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
