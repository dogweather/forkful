---
date: 2024-01-20 17:43:50.698769-07:00
description: "How to: (\u3084\u308A\u65B9) Elixir\u3092\u7528\u3044\u3066HTTP\u306E\
  \u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308A\u3001Web\u30DA\u30FC\u30B8\u306E\
  \u5185\u5BB9\u3092\u53D6\u5F97\u3059\u308B\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\
  \u3087\u3046\u3002`HTTPoison` \u306A\u3069\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.557265-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Elixir\u3092\u7528\u3044\u3066HTTP\u306E\u30EA\u30AF\
  \u30A8\u30B9\u30C8\u3092\u9001\u308A\u3001Web\u30DA\u30FC\u30B8\u306E\u5185\u5BB9\
  \u3092\u53D6\u5F97\u3059\u308B\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\
  \u3002`HTTPoison` \u306A\u3069\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## How to: (やり方)
Elixirを用いてHTTPのリクエストを送り、Webページの内容を取得する例を見てみましょう。`HTTPoison` などのパッケージを使用します。

```elixir
# HTTPoisonパッケージを使う準備
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Webページをダウンロードする関数
def download_web_page(url) do
  case HTTPoison.get(url) do
    {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
      {:ok, body}
    {:ok, %HTTPoison.Response{status_code: status_code}} ->
      {:error, "Failed to fetch. Status code: #{status_code}"}
    {:error, %HTTPoison.Error{reason: reason}} ->
      {:error, "Error: #{reason}"}
  end
end

# 使ってみよう
{:ok, content} = download_web_page("https://example.com")
```

## Deep Dive (深掘り)
過去には`HTTPoison`の前に`HTTPotion`やErlangの`httpc`モジュールが使われていました。`HTTPoison`はErlangの`hackney`ライブラリをラップして使いやすくしています。他の選択肢としては、Elixirの標準ライブラリに含まれる`HTTP`クライアントや最近人気の`Tesla`などもあります。

Webページをダウンロードする際には、サーバーへの負荷や法律、プライバシーへの配慮も必要です。やり過ぎない、適正な規模でのデータ収集を心がけましょう。

## See Also (関連情報)
- HTTPoison GitHub: https://github.com/edgurgel/httpoison
- Hackney GitHub: https://github.com/benoitc/hackney
- Tesla GitHub: https://github.com/teamon/tesla
- Elixir SchoolのHTTP要求: https://elixirschool.com/jp/lessons/libraries/http/
