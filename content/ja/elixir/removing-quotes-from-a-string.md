---
title:                "文字列から引用符を削除する"
date:                  2024-01-26T03:39:32.283867-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列から引用符を剥がすというのは、その余分なラッパーを取り除いて中のクリーントテキストを得ることを意味します。プログラマーがこれを行うのは、入力をサニタイズしたり、エラーを避けたり、引用符が機能ではなく障害となる処理のためにデータを準備するためです。

## 方法：
Elixirには組み込みの「引用符を削除する」機能はありませんが、パターンマッチングや`String`関数を用いて自作するのは簡単です。これらのスニペットを参照してください：

```elixir
# パターンマッチングを使用
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# サンプル使用法
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"

# String.trim/1を使用
def unquote_string(string), do: String.trim(string, "'\"")

# サンプル使用法
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"
```

どちらの方法でも出力は以下になります：
```
"Hello, World!"
```

## ディープダイブ
昔、文字列の中の引用符は地雷原でした—扱いを誤れば、爆発的に構文エラーやセキュリティホールが生じてしまいます。Elixirでは、パターンマッチングが文字列をレゴブロックのように扱い、精度高く分解して再構築させます。その頑丈な`String`モジュールも、`trim`関数で引用符を柔軟に取り除くのに便利です。代替手段としては、正規表現が引用符を退治したり、基本的な剥がし以上の機能が必要な場合は外部ライブラリが追加の火力を提供するかもしれません。

## 参照
これらで深く掘り下げてみてください：
- [ElixirのStringモジュール](https://hexdocs.pm/elixir/String.html)
- [Elixirにおけるパターンマッチングについてもっと学ぶ](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Elixirでの正規表現（Regexモジュール）](https://hexdocs.pm/elixir/Regex.html)