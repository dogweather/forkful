---
title:                "Elixir: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ： 文字列の連結をするのはなぜか？

文字列の連結は、プログラミングで繰り返し使用される一般的なタスクです。文字列を連結することで、プログラムがより効率的に動作し、より見やすくなります。

## 方法：どのように文字列を連結するか？

文字列を連結するためには、Elixirの`<>`演算子を使います。例えば、"Hello"と"World"の文字列を連結するには、`"Hello" <> "World"`という形式でコードを書きます。

```Elixir
"Hello" <> "World"
```

上記のコードを実行すると、"HelloWorld"という文字列が出力されます。

## 深堀り：文字列の連結についてさらに詳しく

Elixirでは、`<>`演算子を使って複数の文字列を一度に連結することができます。例えば、`"Hello" <> " " <> "World"`という形式で、空白を挟んで複数の文字列を連結することもできます。また、変数と文字列を連結する際には、`<>`演算子を使用することで文字列内に変数の値を挿入することができます。

## 参考： 関連リンク

- [Elixirの公式ドキュメント](https://elixir-lang.org/getting-started/basic-types.html#string-interpolation)
- [文字列の連結についてのElixir Forumの記事](https://elixirforum.com/t/concatenation-of-strings/1268)
- [Elixirで文字列を操作する方法についてのブログ記事](https://www.littlelines.com/blog/2014/07/08/elixir-tutorial-how-to-string-manipulation-operators/)
- [ElixirコミュニティのSlackチャンネル](https://elixir-slackin.herokuapp.com/)