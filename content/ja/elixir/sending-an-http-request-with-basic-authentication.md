---
title:                "Elixir: 基本認証を使用したHTTPリクエストの送信"
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

ElixirでHTTPリクエストを基本認証で送信する方法

## Why

Elixirは、動的なWebアプリケーションを構築するための優れたプログラミング言語です。HTTPリクエストを送信することは、Webアプリケーションでデータを取得する必要がある場合には必須です。基本認証を使用することで、セキュリティを強化し、データの取得を制御することができます。

## How To

基本認証を使用してHTTPリクエストを送信する方法を見ていきましょう。

まずは、HTTPヘッダーにBasic認証の情報を追加します。認証には、ユーザー名とパスワードの組み合わせをBase64エンコードした文字列を使用します。

```elixir
headers = [{"Authorization", "Basic #{Base.encode64("<username>:<password>")}}]
```

次に、HTTPクライアントを作成します。Elixirでは、HTTPリクエストを簡単に作成することができるHTTPoisonというライブラリがあります。

```elixir
client = HTTPoison.Client.new()
```

そして、HTTPリクエストを送信します。ここでは、GETメソッドを使用してwww.example.comからデータを取得する例を示します。

```elixir
response = HTTPoison.Client.get(client, "http://www.example.com", headers)
```

最後に、レスポンスを取得します。

```elixir
response.body
```

これで、基本認証を使用してHTTPリクエストを送信することができました。

## Deep Dive

HTTPリクエストを送信する際に、基本認証を使用することで、セキュアな通信を行うことができます。基本認証にはユーザー名とパスワードを平文で送信する欠点がありますが、HTTPSを使用することで情報を暗号化することができます。

また、ElixirではBasic認証の他にも、Digest認証やOAuth認証など様々な認証方法をサポートしています。それぞれの認証方法についても、HTTPリクエストを送信する際に同じようにヘッダーを設定することで使用することができます。

## See Also

こちらのリンクも参考にしてみてください。

- HTTPoisonライブラリのドキュメンテーション：https://hexdocs.pm/httpoison/
- ElixirでのHTTPクライアントの作成方法についてのチュートリアル：https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#creating-an-http-client