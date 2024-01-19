---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# HTTPリクエストと基本認証をElixirで送信する

## それは何？そしてなぜ？
基本認証付きのHTTPリクエスト送信は、ユーザー名とパスワードを利用してウェブサービスに安全にアクセスするための一般的な方法です。プログラマーがこれを行う主な理由は、ユーザーのデータ保護とウェブサービスへのセキュアなアクセスを確保するためです。

## 使い方
以下に、ElixirでHTTPリクエストを送信し、基本認証を行うコードの例を示します。

```Elixir
{:ok, response} = :httpc.request(
  :get,
  {'http://example.com', [{'Authorization', 'Basic bXl1c2VybmFtZTpteXBhc3N3b3Jk'}]},
  [],
  []
)
```
ここで、「bXl1c2VybmFtZTpteXBhc3N3b3Jk」はユーザー名とパスワードをBase64エンコードしたもので、「myusername:mypassword」に相当します。

## Deep Dive
基本認証は、HTTP/1.0が1996年に導入された当初から存在しています。この方法の代替案としては、digest認証やOAuthがあります。

Elixirで基本認証付きHTTPリクエストを送信するためには、Erlangの`:httpc`関数が利用されます。しかし、大規模なプロジェクトでは**HTTPoison**や**Tesla**のようなライブラリがよく利用されます。

## 参考情報
以下は、基本認証とHTTPリクエストについてより深く学べるリソースへのリンクです。

- Elixirの公式ドキュメンテーション: [https://elixir-lang.org/docs.html](https://elixir-lang.org/docs.html)
- HTTPoisonライブラリ: [https://hexdocs.pm/httpoison/readme.html](https://hexdocs.pm/httpoison/readme.html)
- Teslaライブラリ: [https://hexdocs.pm/tesla/readme.html](https://hexdocs.pm/tesla/readme.html)