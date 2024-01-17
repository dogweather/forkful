---
title:                "テキストの検索と置換"
html_title:           "Elixir: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
テキストの検索と置換とは何か、そしてなぜプログラマーがそれを行うのかを説明してみましょう。テキストの検索と置換は、特定のテキストを見つけてそれを別のテキストで置き換えることを指します。プログラマーは、コード内の特定の部分を素早く修正するためにテキストの検索と置換を使用します。

## 方法：
以下のようなコード例を使用して、テキストの検索と置換の方法をご紹介します。
```Elixir
# 文字列内の特定のテキストを置換する
str = "こんにちは、世界！"
str = String.replace(str, "こんにちは", "Hello")
# strは"Hello、世界！"となります。

# 正規表現を使用して複数の文字列を一括置換する
str = "10 apples, 20 oranges, 30 bananas"
str = Regex.replace(str, ~r/\d+/, "5")
# strは"5 apples, 5 oranges, 5 bananas"となります。
```

## 詳しく見ていきましょう：
テキストの検索と置換は、古くから存在する基本的なコンピューターの機能です。プログラム内の特定の部分を見つけて修正するために、プログラマーは検索と置換を使用します。ただし、検索と置換はパターンマッチングや正規表現のような高度な手法を使用することで、より複雑な操作も可能になります。Elixirでは、StringモジュールやRegexモジュールを使用してテキストの検索と置換を行うことができます。

他にも、テキストの検索と置換にはいくつかの代替手段があります。例えば、JavaScriptの正規表現を使用したり、SedやAwkなどのコマンドラインツールを使用したりすることができます。ただし、Elixirは強力なパターンマッチング機能を備えているため、他の言語やツールよりも柔軟で効率的にテキストの検索と置換を行うことができます。

## 関連ソースを参照してみましょう：
- ElixirのStringモジュール: https://hexdocs.pm/elixir/String.html
- ElixirのRegexモジュール: https://hexdocs.pm/elixir/Regex.html
- Sedの使用方法: https://www.gnu.org/software/sed/manual/sed.html
- Awkの使用方法: https://www.gnu.org/software/gawk/manual/gawk.html