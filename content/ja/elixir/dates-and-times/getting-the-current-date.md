---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:47.660476-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.626131-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u306B\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u306E\u65E5\u4ED8\u3068\u6642\u523B\u306E\
  \u60C5\u5831\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u3053\u3068\u304C\u95A2\u308F\
  \u3063\u3066\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30ED\u30B0\u8A18\u9332\
  \u3001\u30C7\u30FC\u30BF\u30B9\u30BF\u30F3\u30D4\u30F3\u30B0\u3001\u307E\u305F\u306F\
  \u73FE\u5728\u306E\u65E5\u4ED8\u306E\u77E5\u8B58\u304C\u5FC5\u8981\u306A\u3042\u3089\
  \u3086\u308B\u6A5F\u80FD\u306E\u305F\u3081\u306B\u3088\u304F\u884C\u308F\u308C\u308B\
  \u30BF\u30B9\u30AF\u3067\u3059\u3002\u3053\u306E\u64CD\u4F5C\u306F\u3001\u6642\u9593\
  \u3092\u610F\u8B58\u3057\u305F\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3092\
  \u4F5C\u6210\u3059\u308B\u305F\u3081\u306B\u4E0D\u53EF\u6B20\u3067\u3042\u308A\u3001\
  \u30A6\u30A7\u30D6\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u306E\u30EC\
  \u30DD\u30FC\u30C8\u751F\u6210\u3084\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u306E\
  \u4F5C\u6210\u3068\u3044\u3063\u305F\u30BF\u30B9\u30AF\u306B\u304A\u3044\u3066\u3082\
  \u91CD\u8981\u3067\u3059\u3002."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 方法：
Elixirの標準ライブラリは、`DateTime`モジュールを通じて、現在の日付と時刻を取得することを可能にします。ElixirはErlang VM（BEAM）上で動作するため、時刻操作のための基底となるErlangの機能を利用します。

### Elixirの標準ライブラリを使用する
Elixirは`DateTime.utc_now/0`関数を提供して、UTCでの現在の日付と時刻を取得します。

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**サンプル出力:**
```
~U[2024-02-05 19:58:40.925931Z]
```

現在の日付だけを取得したい場合は、年、月、日のコンポーネントを抽出することができます：

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**サンプル出力:**
```
~D[2023-05-04]
```

### Timexライブラリを使用する
より複雑な日付と時刻の要求に対しては、Timexという人気のあるサードパーティライブラリを利用できます。まず、mix.exsの依存関係に`Timex`を追加します：

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

依存関係をインストールした後（`mix deps.get`）、Timexを使って現在の日付を取得できます：

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**サンプル出力:**
```
~D[2023-05-04]
```

Timexは日付と時刻の操作に関する広範な機能を提供しており、特にタイムゾーン、フォーマット、日付と時刻のパースを扱う際に、あなたのElixirアプリケーションに強力な追加機能として働きます。

Elixirの組み込み機能とTimexライブラリの両方を理解し、利用することで、Elixirアプリケーションで日付と時刻を簡単に扱えるようになり、アプリケーションのニーズに合わせて精度と使いやすさで体験を調整できます。
