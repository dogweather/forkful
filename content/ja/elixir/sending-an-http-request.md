---
title:                "Elixir: 「HTTPリクエストを送信する」"
simple_title:         "「HTTPリクエストを送信する」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

あなたはElixirプログラミング言語を学ぶことができます。しかし、あなたが日本語を話す場合、それは特に有益であり多くの利点があります。一つの利点は、他の技術分野と間に相互運用性を実現させることができることです。今回は、Elixir言語におけるHTTPリクエストの送信方法についてご紹介します。

## こうやって

まずは、HTTPリクエストを送信する上で必要なライブラリをインストールします。次に、リクエストを送信するために必要な情報を定義します。最後に、 ```HTTPoison``` モジュールを使用してリクエストを送信し、レスポンスを取得します。具体的なコード例を以下に示します。

```
# 必要なライブラリのインストール
mix deps.get

# リクエストを送信するための情報を定義する
url = "https://sampleapi.com/users"
params = %{id: 123, name: "John Doe"}

# リクエストを送信し、レスポンスを取得する
response = HTTPoison.get(url, params)

# レスポンスの状態コードとボディを出力する
IO.inspect response.status_code
IO.inspect response.body
```

上記のコードを実行すると、サンプルのAPIからユーザー情報を取得することができます。このように、Elixir言語を使用することで、HTTPリクエストの送信が簡単に行えるようになります。

## さらに深く

Elixir言語におけるHTTPリクエストの送信方法は、 ```HTTPoison``` モジュールを使用することで実現されています。このモジュールには、さまざまなオプションやクエリパラメータを指定することができ、より詳細なリクエストの管理が可能です。また、同じモジュールを使用してレスポンスのエラーハンドリングも行うことができます。

## 参考リンク

- [Elixir言語公式サイト](https://elixir-lang.org/)
- [HTTPoisonドキュメント](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Elixir Tutorial: Making HTTP Requests Using HTTPoison](https://www.sitepoint.com/elixir-tutorial-getting-started-with-http-requests/)