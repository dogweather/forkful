---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:52.687396-07:00
description: "\u65B9\u6CD5: Elixir\u306F`Regex`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\
  \u4F7F\u3063\u3066regex\u64CD\u4F5C\u3092\u884C\u3044\u307E\u3059\u3002\u3053\u308C\
  \u306FErlang\u306Eregex\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u6D3B\u7528\u3057\u3066\
  \u3044\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\u4F7F\u7528\u4F8B\u3092\u7D39\u4ECB\
  \u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.599600-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u306F`Regex`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u3063\u3066\
  regex\u64CD\u4F5C\u3092\u884C\u3044\u307E\u3059\u3002\u3053\u308C\u306FErlang\u306E\
  regex\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u6D3B\u7528\u3057\u3066\u3044\u307E\u3059\
  \u3002\u57FA\u672C\u7684\u306A\u4F7F\u7528\u4F8B\u3092\u7D39\u4ECB\u3057\u307E\u3059\
  \uFF1A."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

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
