---
title:                "ベーシック認証を使用してhttpリクエストを送信する"
html_title:           "Bash: ベーシック認証を使用してhttpリクエストを送信する"
simple_title:         "ベーシック認証を使用してhttpリクエストを送信する"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを基本認証で送信する理由は、WebサービスやAPIを使う際に、認証を通してセキュリティを確保するためです。

## やり方

```Bash
curl -u username:password https://example.com/api/endpoint
```

このコマンドは、`curl`を使用して基本認証でHTTPリクエストを送信する方法を示しています。`-u`オプションは、ユーザー名とパスワードを指定するために使用します。`username`と`password`は、実際の認証情報に置き換えて使用します。後に、リクエストを送信したいエンドポイントのURLを指定します。

```Bash
HTTP/1.1 200 OK
Content-Type: application/json
Content-Length: 123

{
  "username": "John",
  "email": "john@example.com"
}
```

リクエストに成功した場合、上記のようにレスポンスが返ります。`Content-Type`ヘッダーは、レスポンスの内容がJSON形式であることを示し、`Content-Length`ヘッダーはレスポンスの長さを示しています。そして、実際のレスポンスの内容がJSON形式で返されます。

## ディープダイブ

基本認証は、ユーザー名とパスワードを使用して、認証情報をHTTPリクエストのヘッダーに埋め込むことで機能します。これにより、認証を受ける側のサーバーがユーザーを認識し、アクセスを許可するか拒否するかを決定することができます。基本認証では、認証情報が平文で送信されるため、HTTPSを使用してセキュリティを強化することが推奨されています。

## See Also
- [HTTPリクエストとは？](https://developer.mozilla.org/ja/docs/Web/HTTP/Methods)
- [curlコマンドを使ってHTTPリクエストを送信する方法](https://qiita.com/KawamataL8/items/9fc45dafb78a13206573)