---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何と何故？

Webページのダウンロードは、アクセスしたサイトの内容を自身のコンピュータにコピーする行為を指します。プログラマーはそれを行うことで、オフラインでもそのコンテンツを参照することが可能になり、またデータ解析やスクレイピングのための原材料ともするためです。

## 実行方法：

以下は、ElmでWebページをダウンロードする簡単なコード例です：

```Elm
module Main exposing (..)

import Http
import Json.Decode as Decode

fetchUrl : String -> Cmd Msg
fetchUrl url =
    Http.get
        { url = url
        , expect = Http.expectString GotText
        }

type Msg
    = GotText (Result Http.Error String)

type alias Model =
    String

-- 更新部分
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText (Ok text) ->
            ( text, Cmd.none )

        GotText (Err _) ->
            ( "Error fetching data", Cmd.none )
```
このコードは、指定したURLからWebページをダウンロードし、その内容をテキストとしてString型のモデルに格納します。ダウンロードが失敗すると、エラーメッセージがモデルに格納されます。

## ディープダイブ：

Webページのダウンロードは、初期のインターネットの主要な動作の一つで、ウェブクローラーやスクレイピングツールの基盤技術となっています。代替手段としてWeb APIを利用する方法もありますが、全てのサイトがAPIを提供しているわけではなく、また提供されているAPIが全てのデータをカバーしているわけでもありません。Elmでは、Httpパッケージを利用してこの操作を行います。

詳細については、以下のリンクからElmのHttpパッケージとそのドキュメンテーションを参照してください ：(https://package.elm-lang.org/packages/elm/http/latest/)

## 参考資料：

- Elmの公式ウェブサイト：https://elm-lang.org/
- ElmのHttpパッケージ： https://package.elm-lang.org/packages/elm/http/latest/
- Json.Decodeモジュール：https://package.elm-lang.org/packages/elm/json/latest/Json-Decode