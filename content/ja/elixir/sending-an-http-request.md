---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？
HTTPリクエストを送信するとは、あるウェブサーバーに情報を送ったり、または取得したりすることを意味します。プログラマーがこれを行う主な理由は、ウェブベースのサービスやAPIと対話するためです。

## どうやって：
Elixirで使用できるライブラリの一つ、`httpoison`を用いてHTTPリクエストを作成します。

以下はリクエストの基本的な形式です：
```elixir
HTTPoison.get!("http://example.com").body
```

これにより、指定したURL(`http://example.com`という場合)から情報を取得できます。

対応する出力は次のように表示されます：
```elixir
"<!doctype html><html itemscope=\"\" ..."
```

## ディープダイブ：
HTTPリクエストの歴史には、HTTPプロトコルの発展が含まれています。これは、インターネットという広範囲なネットワークの中で情報を交換するための方法を提供するために開発されました。

Elixirでは、他のHTTPリクエストライブラリもあります。例えば`hackney`や`tesla`などがあります。それぞれのライブラリは、HTTPリクエストを送信するための異なる方法と特徴を提供します。

`httpoison`ライブラリの実装は、底I層でErlangの`hackney`ライブラリを使用しています。これにより、高い性能と柔軟性が提供されます。

## 関連リンク：
1. httpoison ソースコード : [github](https://github.com/edgurgel/httpoison)
2. Elixirでの HTTP リクエスト : [hexdocs](https://hexdocs.pm/httpoison/readme.html)
3. 古代 Unix の HTTP : [httpwg](https://httpwg.org/http-core/)