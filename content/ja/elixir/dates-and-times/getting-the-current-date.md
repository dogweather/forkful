---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:47.660476-07:00
description: "\u65B9\u6CD5\uFF1A Elixir\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306F\u3001`DateTime`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u901A\u3058\u3066\u3001\
  \u73FE\u5728\u306E\u65E5\u4ED8\u3068\u6642\u523B\u3092\u53D6\u5F97\u3059\u308B\u3053\
  \u3068\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002Elixir\u306FErlang VM\uFF08\
  BEAM\uFF09\u4E0A\u3067\u52D5\u4F5C\u3059\u308B\u305F\u3081\u3001\u6642\u523B\u64CD\
  \u4F5C\u306E\u305F\u3081\u306E\u57FA\u5E95\u3068\u306A\u308BErlang\u306E\u6A5F\u80FD\
  \u3092\u5229\u7528\u3057\u307E\u3059\u3002 #."
lastmod: '2024-03-13T22:44:41.626131-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u3001`DateTime`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3092\u901A\u3058\u3066\u3001\u73FE\u5728\u306E\u65E5\u4ED8\
  \u3068\u6642\u523B\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u3092\u53EF\u80FD\u306B\
  \u3057\u307E\u3059\u3002Elixir\u306FErlang VM\uFF08BEAM\uFF09\u4E0A\u3067\u52D5\u4F5C\
  \u3059\u308B\u305F\u3081\u3001\u6642\u523B\u64CD\u4F5C\u306E\u305F\u3081\u306E\u57FA\
  \u5E95\u3068\u306A\u308BErlang\u306E\u6A5F\u80FD\u3092\u5229\u7528\u3057\u307E\u3059\
  ."
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
