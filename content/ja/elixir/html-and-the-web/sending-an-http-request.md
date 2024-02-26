---
date: 2024-01-20 17:59:23.777288-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3044\
  \u3046\u306E\u306F\u3001\u30A6\u30A7\u30D6\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\
  \u3092\u6C42\u3081\u305F\u308A\u64CD\u4F5C\u3092\u6307\u793A\u3057\u305F\u308A\u3059\
  \u308B\u65B9\u6CD5\u3060\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u306E\
  \u4F5C\u696D\u3092\u884C\u3046\u306E\u306F\u3001\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\
  \u3057\u305F\u308A\u30A6\u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\u3068\u9023\u643A\u3057\
  \u305F\u308A\u3059\u308B\u305F\u3081\u3060\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:39.760494-07:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3044\u3046\
  \u306E\u306F\u3001\u30A6\u30A7\u30D6\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\
  \u6C42\u3081\u305F\u308A\u64CD\u4F5C\u3092\u6307\u793A\u3057\u305F\u308A\u3059\u308B\
  \u65B9\u6CD5\u3060\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u306E\u4F5C\
  \u696D\u3092\u884C\u3046\u306E\u306F\u3001\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\
  \u305F\u308A\u30A6\u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\u3068\u9023\u643A\u3057\u305F\
  \u308A\u3059\u308B\u305F\u3081\u3060\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

HTTPリクエストを送るっていうのは、ウェブサーバーに情報を求めたり操作を指示したりする方法だ。プログラマーがこの作業を行うのは、データを取得したりウェブサービスと連携したりするためだ。

## How to: (やり方)

```elixir
# HTTPoisonを使ってGETリクエストを送る
HTTPoison.start()
{:ok, response} = HTTPoison.get("https://api.example.com/data")

# responseの内容を表示
IO.inspect(response)

# POSTリクエストを送る例
headers = [{"Content-Type", "application/json"}]
body = Jason.encode!(%{key: "value"})

{:ok, response} = HTTPoison.post("https://api.example.com/data", body, headers)

# responseの内容を表示
IO.inspect(response)
```

このコードはHTTPoisonライブラリを使っている。GETとPOSTの両リクエストの応答をターミナルに出力する。

## Deep Dive (深掘り)

HTTPリクエスト送信の基本は、常に変わらないわけではない。Elixirが生まれる前、Erlangや他の言語では異なるツールやライブラリを使っていた。今でもHTTPリクエストは標準ライブラリの`:httpc`を使ったり、TeslaやHTTPotionのような他のライブラリを使う選択肢がある。HTTPoisonはErlangの`:hackney`ライブラリを基に作られており、使いやすさと拡張性を兼ね備えているのが特徴だ。

実装の詳細については、HTTPoisonを始める前に`:hackney`、それに`HTTPoison.start()`を呼ぶ必要がある。これは`:hackney`をElixirのアプリケーションとして起動するための準備だ。また、リクエスト時には`headers`と`body`を明示的に定義し、正確なHTTPリクエストを構築できる。

## See Also (関連情報)

- [HTTPoison GitHub repository](https://github.com/edgurgel/httpoison)
- [Hex.pm package for HTTPoison](https://hex.pm/packages/httpoison)
- [Erlang :httpc documentation](http://erlang.org/doc/man/httpc.html)
- [Jason Hex package for JSON handling in Elixir](https://hex.pm/packages/jason)
