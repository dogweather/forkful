---
title:                "HTTPリクエストを送信する"
html_title:           "Elixir: HTTPリクエストを送信する"
simple_title:         "HTTPリクエストを送信する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何？なぜ？

HTTPリクエストを送信することは、インターネット上の情報を要求するための方法です。プログラマーは、外部のWebサービスやAPIからデータを取得するためにHTTPリクエストを使用することができます。

## 方法：

```Elixir
httpc.get("https://api.example.com/users") 
```
このコードは、`httpc`モジュールを使用して、指定されたURLからデータを取得するHTTP GETリクエストを送信します。また、成功した場合は、レスポンスのステータスコードとヘッダーを出力します。

## 詳細を深く調べる：

HTTPリクエストは、Webの発展とともに誕生しました。最も一般的な方法の一つであるGETリクエストは、ユーザーがWebページを表示するために使用されます。HTTPリクエストの代替としては、POST、PUT、PATCH、およびDELETEもあります。

HTTPリクエストの実装には、Elixirの`httpc`モジュール以外にも、HTTPoisonやTeslaなどのパッケージがあります。これらのパッケージは、さまざまな機能と使いやすいAPIを提供しています。

## 関連リンク：

- [Elixirの`httpc`モジュールの公式ドキュメント](https://hexdocs.pm/elixir/HTTPotion.html)
- [HTTPoisonパッケージのGitHubリポジトリ](https://github.com/edgurgel/httpoison)
- [TeslaパッケージのGitHubリポジトリ](https://github.com/teamon/tesla)