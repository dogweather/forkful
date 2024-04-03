---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:52.687396-07:00
description: "Elixir\u3067\u306E\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u3001\
  \u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u306B\u57FA\u3065\u3044\u3066\u6587\u5B57\
  \u5217\u3092\u691C\u7D22\u3001\u4E00\u81F4\u3055\u305B\u3001\u64CD\u4F5C\u3059\u308B\
  \u305F\u3081\u306B\u4F7F\u308F\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u6587\u5B57\u5217\u51E6\u7406\u306E\u52B9\u7387\u6027\u3068\u6C4E\
  \u7528\u6027\u306E\u304A\u304B\u3052\u3067\u3001\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\
  \u306E\u691C\u8A3C\uFF08\u30E1\u30FC\u30EB\u3001URL\uFF09\u3001\u30ED\u30B0\u306E\
  \u89E3\u6790\u3001\u30C7\u30FC\u30BF\u62BD\u51FA\u306A\u3069\u306E\u30BF\u30B9\u30AF\
  \u306Bregex\u3092\u6D3B\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.599600-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u3067\u306E\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u3001\u7279\
  \u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u306B\u57FA\u3065\u3044\u3066\u6587\u5B57\u5217\
  \u3092\u691C\u7D22\u3001\u4E00\u81F4\u3055\u305B\u3001\u64CD\u4F5C\u3059\u308B\u305F\
  \u3081\u306B\u4F7F\u308F\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u6587\u5B57\u5217\u51E6\u7406\u306E\u52B9\u7387\u6027\u3068\u6C4E\u7528\
  \u6027\u306E\u304A\u304B\u3052\u3067\u3001\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306E\
  \u691C\u8A3C\uFF08\u30E1\u30FC\u30EB\u3001URL\uFF09\u3001\u30ED\u30B0\u306E\u89E3\
  \u6790\u3001\u30C7\u30FC\u30BF\u62BD\u51FA\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306B\
  regex\u3092\u6D3B\u7528\u3057\u307E\u3059\u3002."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 何となぜ？

Elixirでの正規表現（regex）は、特定のパターンに基づいて文字列を検索、一致させ、操作するために使われます。プログラマーは、文字列処理の効率性と汎用性のおかげで、フォーマットの検証（メール、URL）、ログの解析、データ抽出などのタスクにregexを活用します。

## 方法:

Elixirは`Regex`モジュールを使ってregex操作を行います。これはErlangのregexライブラリを活用しています。基本的な使用例を紹介します：

```elixir
# パターンに一致する - 最初の一致を返す
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # 出力: ["hello"]

# すべての一致を見つける
all_matches = Regex.scan(~r/\d/, "There are 2 apples and 5 oranges.")
IO.inspect(all_matches) # 出力: [["2"], ["5"]]

# 文字列の一部を置換する
replaced_string = Regex.replace(~r/\s+/, "Elixir is fun", "_")
IO.inspect(replaced_string) # 出力: "Elixir_is_fun"
```

より複雑なパターンや機能が必要な場合は、サードパーティのライブラリを検討するかもしれませんが、多くの基本的な文字列およびパターンマッチングのタスクには、Elixirの組み込み`Regex`モジュールが非常に強力です。

大文字小文字を区別しない一致を実行するには、`i`オプションを使用します：

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # 出力: ["Hello"]
```

複数回使用する場合の効率を上げるため、正規表現を事前にコンパイルすることができます：

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # 出力: ["hello"]
```

Elixirは、文字列の特定の部分を抽出しながらコードをより読みやすくするのに非常に便利な、名前付きキャプチャもサポートしています：

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # 出力: %{"year" => "2023", "month" => "04", "day" => "15"}
```

この簡単な概要は、Elixirが正規表現をどれだけ簡単に扱うか、そして強力な文字列操作およびデータ抽出技術を可能にすることを強調しています。
