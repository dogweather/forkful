---
title:                "httpリクエストを送信する"
html_title:           "Elm: httpリクエストを送信する"
simple_title:         "httpリクエストを送信する"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?:
HTTPリクエストを送信するとは、ウェブサイトやウェブアプリケーションから情報を取得することです。プログラマーは、異なるウェブサービスやデータにアクセスするためにHTTPリクエストを使用します。

## How to:
Elmでは、Http.get関数を使用してHTTPリクエストを送信することができます。この関数には、リクエストのURLと期待するレスポンスの型を指定します。例えば、GitHub APIからリポジトリの情報を取得するには、以下のようにコードを書きます。

```Elm
Http.get "https://api.github.com/users/username/repos" RepoList
```

このコードを実行すると、以下のようなリストが得られます。

```Elm
[
  { name = "repo1", description = "A repository for testing", url = "https://github.com/username/repo1" },
  { name = "repo2", description = "Another repository", url = "https://github.com/username/repo2" }
]
```

## Deep Dive:
HTTPリクエストはウェブ開発において重要な役割を果たしています。ウェブサービスを使用するためには、HTTPリクエストを送信する必要があります。代表的なHTTPメソッドとしては、GET、POST、PUT、DELETEなどがあります。

ただし、ElmにはHTTPリクエストを送信する代替手段もあります。例えば、Json.Decoderモジュールを使用して、手動でHTTPリクエストを処理することもできます。また、JavaScriptとの統合を行うことで、より柔軟なHTTPリクエストの処理が可能になります。

HTTPリクエストは、ユーザーから特定のウェブサービスに対してデータを送信する際にも使用されます。例えば、フォームからの入力データをウェブサービスに送信する際には、HTTPリクエストを使用します。

## See Also:
- https://guide.elm-lang.org/effects/http.html#http
- https://www.w3schools.com/tags/ref_httpmethods.asp
- https://package.elm-lang.org/packages/elm/json/latest/Json-Decoder