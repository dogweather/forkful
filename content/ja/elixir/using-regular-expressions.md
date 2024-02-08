---
title:                "正規表現の使用"
aliases:
- ja/elixir/using-regular-expressions.md
date:                  2024-02-03T19:16:52.687396-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
