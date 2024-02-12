---
title:                "テキストの検索と置換"
aliases:
- /ja/elixir/searching-and-replacing-text.md
date:                  2024-01-20T17:57:45.118640-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストの検索と置換は、ある文字列を別の文字列で置き換える処理です。プログラマーは、コード内の値の更新、データの修正、あるいはテキストファイル内の情報の一括変更などのためにこれを使います。

## How to: (方法)
```elixir
# 文字列内で単語を探して置換
original_text = "ちょっとしたElixirの魔法"
replaced_text = String.replace(original_text, "魔法", "マジック")
IO.puts replaced_text
# 出力: ちょっとしたElixirのマジック

# 正規表現を使ってパターンにマッチするテキストを置換
regex = ~r/魔法{1,}/u
updated_text = Regex.replace(regex, original_text, "チャーム")
IO.puts updated_text
# 出力: ちょっとしたElixirのチャーム
```

## Deep Dive (深い解析)
テキストの検索と置換は、多くのプログラミング言語で使われる基本的な操作です。Elixirでは、`String.replace/4`や`Regex.replace/4`といった関数を使用します。Pythonにおける`str.replace()`やRubyの`String#gsub`と似ていますが、Elixirは不変性を持つため、オリジナルの文字列は変更されません。また、`Regex`モジュールを使用して、正規表現を利用した複雑なパターンマッチングと置換が可能です。Elixirのパターンマッチング能力には、パイプラインを使ってデータ処理の流れを表現する機能などがあり、これによりより洗練された操作が行えます。

## See Also (更に参照)
- Elixirの`String`モジュール: https://hexdocs.pm/elixir/String.html
- `Regex`モジュール: https://hexdocs.pm/elixir/Regex.html
- 正規表現に関するオンラインチュートリアル: https://www.regular-expressions.info/tutorial.html
- Elixir School（Elixirの学習リソース）: https://elixirschool.com/jp/
