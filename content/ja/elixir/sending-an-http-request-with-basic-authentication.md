---
title:                "基本認証を使用してhttpリクエストを送信する."
html_title:           "Elixir: 基本認証を使用してhttpリクエストを送信する."
simple_title:         "基本認証を使用してhttpリクエストを送信する."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

HTTPリクエストを基本認証付きで送信するとは何かと、プログラマーがそれをする理由について説明します。

HTTPリクエストを基本認証付きで送信するとは、サーバーに対してユーザー名とパスワードを用いて認証を行い、データを送受信することです。プログラマーは、これを行うことでセキュリティを確保し、プライベートな情報を保護することができます。

## How to:

以下に、Elixirを使用してHTTPリクエストを基本認証付きで送信する方法を示します。

```
# HTTPライブラリをインポート
import Http

# リクエストを作成
request = %Http.Request{
  method: :get,
  url: "https://www.example.com",
  basic_auth: {"username", "password"}
}

# リクエストを送信
response = Http.request(request)

# レスポンスを取得
response.body
```

このコードを実行すると、 `https://www.example.com` に対して基本認証付きでHTTP GETリクエストが送信されます。レスポンスの本文は、 `response.body` を使用して取得することができます。

## Deep Dive:

基本認証は、HTTPプロトコルの一部になっています。これは、クライアントがサーバーに対してユーザー名とパスワードを送信し、サーバーがそれを認証することで、通信が安全に行われることを保証する仕組みです。

基本認証は、古くから存在している認証方式であり、多くのWebアプリケーションでも使用されています。しかし、最近ではより高度な認証方式が開発されており、基本認証の使用は推奨されません。

Elixirには、基本認証の他にもさまざまな認証方式をサポートするライブラリが存在します。しかし、基本認証はシンプルで使いやすいため、小規模なサーバーやAPIなど、シンプルな認証が必要な場合には有用です。

## See Also:

- ElixirのHTTPクライアントライブラリ: https://hexdocs.pm/httpoison/Httpoison.html
- HTTP基本認証の詳細: https://www.w3.org/Protocols/HTTP/Authentication.html