---
title:                "基本認証を使ったhttpリクエストの送信"
html_title:           "Gleam: 基本認証を使ったhttpリクエストの送信"
simple_title:         "基本認証を使ったhttpリクエストの送信"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なに & なぜ？
HTTPリクエストを基本認証で送信するとは、Webプログラマーがよくやることのひとつです。この手法を使う主な理由は、セキュリティを強化し、アクセス制限をかけるためです。

## 方法：
以下のような ```Gleam ... ```のコードブロックを使用して、基本認証でHTTPリクエストを送信する方法を示します。また、サンプルの出力も表示します。

```Gleam

let Response =
  Gleam.Http.send(
    "GET",
    "https://example.com/api",
    [
      ("Authorization", "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
    ],
    Gleam.Http.empty_body()
  )

case Response {
  Ok(res) ->
    Gleam.IO.print(String.append("Response code: ", res.status_code))
    Gleam.IO.println(String.append("Response body: ", res.body))
  Err(err) ->
    Gleam.IO.println(String.append("Error: ", err))
}  
```

出力例：
```
Response code: 200
Response body: {"name": "John", "age": 33}
```

## ディープダイブ：
基本認証は、1996年にRFC 2617として標準化されました。代替手法としては、Bearerトークンを使用するOAuthや、APIキーを使うAPIキー認証などがあります。基本認証では、リクエストヘッダーにユーザー名とパスワードを含めて送信します。サーバー側では、この情報を使ってアカウントが有効かどうかを確認し、認証を行います。

## 関連情報：
- [RFC 2617: HTTP Authentication: Basic and Digest Access Authentication](https://rocket.rs/v0.4/guide/requests/#basic-authentication)
- [OAuth.net: OAuth 2.0 Introduction](https://oauth.net/2/)
- [APIキー認証について学ぶ – AWSドキュメント](https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/api-gateway-api-key-source.html)