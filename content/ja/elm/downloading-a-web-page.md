---
title:                "ウェブページのダウンロード"
html_title:           "Elm: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

概要＆理由：
Webページのダウンロードとは、インターネット上からコンピュータにデータを取得することです。プログラマーたちは、Webページをダウンロードすることで、ユーザーに情報を提供することができるようにしています。

方法：
```Elm
import Http
import Json.Decode as Json

type alias Page =
  { title : String
  , content : String
  }

pageDecoder : Json.Decoder Page
pageDecoder =
  Json.map2 Page
    (Json.field "title" Json.string)
    (Json.field "content" Json.string)

getPage : Cmd Msg
getPage =
  Http.get
    { url = "https://example.com/page"
    , expect = Http.expectJson pageDecoder PageReceived
    }

type Msg
  = PageReceived (Result Http.Error Page)

```

詳細：
Webページのダウンロードは、現代のWebアプリケーション開発において欠かせない機能です。HTTPプロトコルを使用して、サーバーからデータを取得し、クライアントのコンピュータに表示することができます。

代替手段としては、バックエンドプログラミング言語（例：PHP、Ruby、Python）を使用して、サーバー側でWebページを生成し直接クライアントに提供する方法があります。

WebページのダウンロードはElmでも非常にカンタンに実装できます。例えば、```Http.get```関数を使用することで、サーバーからデータを取得し、JSONフォーマットのレスポンスをデコードすることができます。

関連情報：
- Elm Documentation (https://guide.elm-lang.org/effects/http.html)
- HTTP Protocol (https://developer.mozilla.org/ja/docs/Web/HTTP)
- JSON Data Format (https://www.json.org/)