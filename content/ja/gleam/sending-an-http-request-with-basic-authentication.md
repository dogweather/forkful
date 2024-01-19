---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストを基本認証で送信するというのは何か、及びプログラマがそれを行う理由について説明します。これは、特定のウェブベースのリソースへのアクセスを認証するための一般的な方法であり、ユーザ名とパスワードを用いてセキュリティを強化します。

## どうやって：

以下はGleam言語を用いた例です：

```Gleam
import gleam/httpc.{Response}
import gleam/uri.Uri
import gleam/http.{Get, Headers, header}
import gleam/httpc

let uri = Uri.parse("https://www.example.com").unwrap()
let headers = Headers.empty
  |> header.add("Authorization", "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
let request = Get(uri, headers)

match httpc.send(request) {
   Ok(Response(200, headers, body)) ->
     body
   Ok(Response(code, headers, body)) ->
     ""
   Error(e) ->
     "Request failed"
}
```

このコードは、基本認証を使用してHTTPリクエストを送信するものです。実行結果は以下のとおりです：

```Gleam
"Request successful"
```

## ディープダイブ：

基本認証を使用してHTTPリクエストを送信する事の歴史的な文脈、代替手段、実装の詳細について探りましょう。

基本認証は、HTTPプロトコルが初めて導入された頃から存在しています。最初は使いやすさを目指した機能でしたが、その後、効果的なセキュリティ手段として認識されるようになりました。

その方法としては、トークンベースの認証やOAuthなどの他の認証手段があります。これらは異なるセキュリティ要件や使用ケースに対応します。

基本認証の実装は、`Authorization`ヘッダーに`Basic `に続いてBase64でエンコードされたユーザ名とパスワードを追加することで行われます。注意点としては、基本認証は透過的なため、通信が傍受されるとパスワードが漏れる可能性があるため、HTTPSを使用して通信を暗号化することが強く推奨されます。

## 参考資料:

1. [Gleam httpc](https://docs.rs/gleam_httpc/0.3.1/gleam_httpc/)
2. [HTTP Basic Authentication](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
3. [Gleam Programming Language](https://gleam.run/)