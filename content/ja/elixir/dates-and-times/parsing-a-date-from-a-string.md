---
aliases:
- /ja/elixir/parsing-a-date-from-a-string/
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 02:05:13.541724-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u306F\u3001\"2023-04-05\"\u306E\u3088\u3046\u306A\u30C6\u30AD\u30B9\u30C8\
  \u3092\u53D6\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u7406\u89E3\u3057\u3001\
  \u6271\u3048\u308B\u65E5\u4ED8\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306B\u5909\u63DB\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\
  \u3053\u308C\u3092\u884C\u3046\u306E\u306F\u3001\u65E5\u4ED8\u304C\u591A\u69D8\u306A\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u5B58\u5728\u3057\u3001\u305D\u308C\u3089\
  \u3092\u9069\u5207\u306B\u6BD4\u8F03\u3001\u30BD\u30FC\u30C8\u3001\u307E\u305F\u306F\
  \u4FDD\u5B58\u3059\u308B\u305F\u3081\u306B\u4E00\u8CAB\u6027\u304C\u5FC5\u8981\u3060\
  \u304B\u3089\u3067\u3059\u3002"
lastmod: 2024-02-18 23:08:54.651972
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u306F\u3001\"2023-04-05\"\u306E\u3088\u3046\u306A\u30C6\u30AD\u30B9\u30C8\
  \u3092\u53D6\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u7406\u89E3\u3057\u3001\
  \u6271\u3048\u308B\u65E5\u4ED8\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306B\u5909\u63DB\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\
  \u3053\u308C\u3092\u884C\u3046\u306E\u306F\u3001\u65E5\u4ED8\u304C\u591A\u69D8\u306A\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u5B58\u5728\u3057\u3001\u305D\u308C\u3089\
  \u3092\u9069\u5207\u306B\u6BD4\u8F03\u3001\u30BD\u30FC\u30C8\u3001\u307E\u305F\u306F\
  \u4FDD\u5B58\u3059\u308B\u305F\u3081\u306B\u4E00\u8CAB\u6027\u304C\u5FC5\u8981\u3060\
  \u304B\u3089\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から日付を解析するとは、"2023-04-05"のようなテキストを取り、プログラムが理解し、扱える日付フォーマットに変換することです。プログラマーがこれを行うのは、日付が多様なフォーマットで存在し、それらを適切に比較、ソート、または保存するために一貫性が必要だからです。

## 方法：

Elixirでは、`Date`モジュールを使って日付を解析できます。文字列を日付に変換する方法は以下の通りです：

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

サンプル出力：

```elixir
~D[2023-04-05]
```

異なるフォーマットを扱うには、`Timex`ライブラリを使用できます：

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

サンプル出力：

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## 深掘り

`Date.from_iso8601/1`関数は、ISO8601日付標準（一般的な日付フォーマット）の簡単な解析を保証するために導入された、Elixirの標準ライブラリの一部です。しかし人生はそんなに単純ではありません。日付は多様なフォーマットで存在します。ここで、サードパーティのElixirライブラリである`Timex`が登場します。`Timex`は、組み込みのElixir日付関数よりも豊富であり、さまざまな日付フォーマットを扱うのに役立ちます。

Elixir自体は不変であり、解析された日付も例外ではありません。一度作成されたら変更できません。この機能は、Elixirの関数型プログラミングのルーツに立ち返り、予測可能性とデバッグの容易さを保証します。

歴史的に、日付の解析は様々な標準が存在するため困難でした。しかし、`Timex`のようなライブラリやElixirの言語機能を使用することで、複雑さが抽象化され、開発者の生活が少し簡単になります。

## 参照

- [Elixir日付](https://hexdocs.pm/elixir/Date.html)
- [Timexドキュメント](https://hexdocs.pm/timex/Timex.html)
- [ISO8601標準](https://www.iso.org/iso-8601-date-and-time-format.html)
