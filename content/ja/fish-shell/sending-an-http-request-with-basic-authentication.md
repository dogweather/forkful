---
title:                "Fish Shell: 基本認証を使用してHTTPリクエストを送信する"
simple_title:         "基本認証を使用してHTTPリクエストを送信する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを基本認証付きで送信することの *なぜ* を説明するため、たった1-2文を使います。

基本認証とは、Webサーバーにアクセスする際にユーザー名とパスワードを使って認証する仕組みです。この認証方法は、ウェブサイトやAPIに安全にアクセスし、プライバシーを保護するために非常に重要です。

## ハウツー

基本認証付きのHTTPリクエストを送信する方法を説明します。以下のようなコードブロックを使用して、Fish Shellコードの例とサンプル出力を示します。

```Fish Shell
# ユーザー名とパスワードを変数に設定
set -x username {your username}
set -x password {your password}

# リクエストを作成
set -x request (curl -u $username:$password -H "Content-Type: application/json" -X GET {API URL})

# レスポンスを出力
echo $request
```

このコードは、基本認証を使用してAPIからデータを取得するシンプルな例です。これにより、必要な認証情報を提供して、リクエストを送信することができます。

## ディープダイブ

基本認証を使用してHTTPリクエストを送信する方法についてより詳細に説明します。基本認証は、安全な通信をするために使用される一般的な認証方法です。基本認証は、ユーザー名とパスワードをリクエストヘッダーに含め、エンコードされたテキストとして送信することで動作します。

また、基本認証を使用する際には、HTTPSを使用して通信することが推奨されます。HTTPSは、リクエストを暗号化し、プライバシーを保護することができます。

## 参考リンク

- [HTTP Authentication Basics](https://tools.ietf.org/html/rfc7617)
- [cURL Documentation](https://curl.se/docs/manpage.html)
- [How Basic Authentication works](https://www.digitalocean.com/community/tutorials/how-basic-authentication-works-in-http)
- [HTTPS explained](https://www.cloudflare.com/learning/ssl/what-is-https/)