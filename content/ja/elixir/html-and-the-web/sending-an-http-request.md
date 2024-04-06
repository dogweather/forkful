---
date: 2024-01-20 17:59:23.777288-07:00
description: "How to: (\u3084\u308A\u65B9) \u3053\u306E\u30B3\u30FC\u30C9\u306FHTTPoison\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3063\u3066\u3044\u308B\u3002GET\u3068POST\u306E\
  \u4E21\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u5FDC\u7B54\u3092\u30BF\u30FC\u30DF\u30CA\
  \u30EB\u306B\u51FA\u529B\u3059\u308B\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.554293-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u3053\u306E\u30B3\u30FC\u30C9\u306FHTTPoison\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3063\u3066\u3044\u308B\u3002GET\u3068POST\u306E\
  \u4E21\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u5FDC\u7B54\u3092\u30BF\u30FC\u30DF\u30CA\
  \u30EB\u306B\u51FA\u529B\u3059\u308B\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
