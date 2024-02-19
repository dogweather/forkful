---
aliases:
- /ja/elixir/downloading-a-web-page/
date: 2024-01-20 17:43:50.698769-07:00
description: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\
  \u308B\u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\u30DA\
  \u30FC\u30B8\u3092\u53D6\u5F97\u3057\u3001\u30ED\u30FC\u30AB\u30EB\u306E\u30B3\u30F3\
  \u30D4\u30E5\u30FC\u30BF\u3084\u30B5\u30FC\u30D0\u30FC\u306B\u4FDD\u5B58\u3059\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\
  \u30FC\u30BF\u62BD\u51FA\u3001\u30B3\u30F3\u30C6\u30F3\u30C4\u306E\u30D0\u30C3\u30AF\
  \u30A2\u30C3\u30D7\u3001\u30AA\u30D5\u30E9\u30A4\u30F3\u5206\u6790\u306E\u305F\u3081\
  \u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.641906
model: gpt-4-1106-preview
summary: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\
  \u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\u30DA\u30FC\
  \u30B8\u3092\u53D6\u5F97\u3057\u3001\u30ED\u30FC\u30AB\u30EB\u306E\u30B3\u30F3\u30D4\
  \u30E5\u30FC\u30BF\u3084\u30B5\u30FC\u30D0\u30FC\u306B\u4FDD\u5B58\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\
  \u30BF\u62BD\u51FA\u3001\u30B3\u30F3\u30C6\u30F3\u30C4\u306E\u30D0\u30C3\u30AF\u30A2\
  \u30C3\u30D7\u3001\u30AA\u30D5\u30E9\u30A4\u30F3\u5206\u6790\u306E\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
Webページをダウンロードするとは、インターネット上のページを取得し、ローカルのコンピュータやサーバーに保存することです。プログラマーは、データ抽出、コンテンツのバックアップ、オフライン分析のためにこれを行います。

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
