---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:01:31.802142-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストに基本認証を使うとは、ユーザー名とパスワードを使ってサーバーへのアクセスを許可することです。これにより、認証が必要な情報へ安全にアクセスできるようになります。

## How to: (方法)
```gleam
import gleam/http
import gleam/httpc
import gleam/base64

pub fn send_auth_request() {
  let username = "yourUsername"
  let password = "yourPassword"
  let credentials = base64.encode(username ++ ":" ++ password)
  let headers = [http.Header("Authorization", "Basic " ++ credentials)]
  let request = http.Request(
    method: http.Get,
    url: "https://example.com/protected",
    headers: headers,
    body: http.Body.None,
  )

  case httpc.send(request) {
    Ok(response) -> 
      io.println("Success! Response: " ++ response.body)
    Error(error) -> 
      io.println("Oops! An error occurred: " ++ error)
  }
}
```

実行結果:
```
Success! Response: Here is your protected resource.
```

## Deep Dive (深く掘り下げる)
基本認証は、HTTP 1.0で登場し、RFC 7617で定義されています。ユーザー名とパスワードはコロンで結合し、Base64でエンコードしてヘッダーに含めます。セキュリティが強くないため、HTTPSと組み合わせて使われるのが一般的です。

選択肢として、より安全なOAuthやTokenベース認証も存在します。しかし、小規模あるいは内部使用時には基本認証が簡単で十分かもしれません。

Gleamでは、httpcモジュールを使ってHTTPリクエストを送ることができ、これには基本認証を含む機能があります。重要なのは、認証情報を安全に扱い、平文で保存や転送をしないことです。

## See Also (関連情報)
- [Basic access authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [RFC 7617, The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
