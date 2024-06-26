---
date: 2024-01-20 17:36:32.580204-07:00
description: "How to (\u65B9\u6CD5) \u30C7\u30D5\u30A9\u30EB\u30C8\u3067\u306FISO8601\u5F62\
  \u5F0F\u3067\u3059\u304C\u3001\u30AB\u30B9\u30BF\u30E0\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u3082\u5229\u7528\u3067\u304D\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:37:49.956598-06:00'
model: gpt-4-1106-preview
summary: "How to (\u65B9\u6CD5) \u30C7\u30D5\u30A9\u30EB\u30C8\u3067\u306FISO8601\u5F62\
  \u5F0F\u3067\u3059\u304C\u3001\u30AB\u30B9\u30BF\u30E0\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u3082\u5229\u7528\u3067\u304D\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to (方法)
```Elixir
# Elixirの内蔵ライブラリを使った日付を文字列に変換
date = ~D[2023-04-05]
formatted_date = to_string(date)

IO.puts(formatted_date) # 出力: "2023-04-05"

# カスタムフォーマットを使用する
custom_format = "{YYYY}-{M}-{D}"
custom_formatted_date = Date.to_string(date, custom_format)

IO.puts(custom_formatted_date) # 出力: "2023-4-5"
```
デフォルトではISO8601形式ですが、カスタムフォーマットも利用できます。

## Deep Dive (深い潜在)
Elixirの日付と文字列変換は、Erlangのカレンダー機能に基づいています。現代のElixirバージョンでは、Dateモジュールを使用して簡単に日付を文字列へ（その逆も）変換できます。

以前のバージョンでは、外部ライブラリに依存する必要がありましたが、現在は標準ライブラリで完結しています。`Date.to_string/1`関数はISO8601フォーマットを使い、`Date.to_string/2`はカスタム形式を受け入れます。フォーマト指定子は `{YYYY}`, `{M}`, `{D}` のように使われ、それぞれ年、月、日を表します。

また、自然言語処理ライブラリやカスタムの日付関連ライブラリを使用することも可能です。これらはより高度なパターンや言語の特性に基づくフォーマットを提供しますが、ほとんどの基本的な用途にはElixirの標準機能で十分です。

## See Also (関連情報)
- Elixirの公式ドキュメント: [Date](https://hexdocs.pm/elixir/Date.html)
- Erlangのカレンダー機能: [Erlang -- calendar](http://erlang.org/doc/man/calendar.html)
- ISO8601についての詳細: [Wikipedia ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
