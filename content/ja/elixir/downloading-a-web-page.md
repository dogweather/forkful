---
title:                "ウェブページのダウンロード"
date:                  2024-01-20T17:43:50.698769-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/downloading-a-web-page.md"
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
