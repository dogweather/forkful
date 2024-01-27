---
title:                "正規表現の使用"
date:                  2024-01-19
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
正規表現は、文字列パターンを定義する方法。プログラマーは、検索、置換、データ検証など多様なタスクを簡単に行うために使用します。

## How to: (方法)
Elixirでの正規表現の使用例：

```elixir
# 文字列検索
regex = ~r/hello/
"hello world" =~ regex
# 結果: true

# 置換
"hello world" |> String.replace(~r/[eo]/, "*")
# 結果: "h*ll* w*rld"

# キャプチャと置換
String.replace("hello world", ~r/(hello)/, fn _ ->
  "bonjour"
end)
# 結果: "bonjour world"
```

## Deep Dive (詳細情報)
Elixirは、Erlangの正規表現ライブラリを内製している。古い選択肢としては文字列関数があるが、正規表現の方が柔軟性が高い。Elixirのマクロにより、正規表現はコンパイル時に作られ、実行時の性能が向上する。

## See Also (関連情報)
- Elixirの正規表現に関する公式ドキュメント: [Elixir Regex Docs](https://hexdocs.pm/elixir/Regex.html)
- 正規表現についてのガイド: [Regular Expressions.info](https://www.regular-expressions.info/)
- インタラクティブな正規表現のテスト: [Rubular](http://rubular.com/)
