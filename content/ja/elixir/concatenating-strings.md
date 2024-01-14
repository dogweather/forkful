---
title:                "Elixir: 文字列の連結"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結することの重要性について考えたことはありますか？プログラミング言語によっては、文字列の結合をするには複雑な手順が必要ですが、Elixirではとても簡単です。この記事では、なぜElixirを使って文字列の結合をするべきなのかについて説明します。

## 方法

では、早速Elixirで文字列の結合をしてみましょう。下記のコードを見てください。

```Elixir
name = "太郎"
greeting = "こんにちは、"
full_greeting = greeting <> name
IO.puts full_greeting
```

このコードでは、`<>`演算子を使って`greeting`と`name`を連結することができます。`IO.puts`関数を使うことで、コンソールに連結された文字列が表示されます。

下記の出力結果をご覧ください。

```
こんにちは、太郎
```

もう少し複雑な例を見てみましょう。下記のコードを見てください。

```Elixir
first_name = "太郎"
last_name = "山田"
greeting = "こんにちは、"
full_name = first_name <> " " <> last_name
full_greeting = greeting <> full_name
IO.puts full_greeting
```

この例では、`first_name`と`last_name`を先に結合し、`full_name`という新しい変数として保存してから、`greeting`と結合します。このように、2つ以上の文字列を連結することもできます。下記の出力結果をご覧ください。

```
こんにちは、太郎 山田
```

## 深堀り

Elixirでは、2つ以上の文字列を連結する場合には、`<>`演算子を使う必要があります。また、`<>`演算子を使うことで、文字列以外のデータも連結することができます。例えば、下記のコードでは、`22`という整数を`"This is the number: "`という文字列に連結しています。

```Elixir
phrase = "This is the number: " <> 22
IO.puts phrase
```

出力結果は次の通りです。

```
This is the number: 22
```

また、`String.length`関数を使うことで、連結された文字列の長さを確認することができます。

```Elixir
full_name = "太郎山田"
IO.puts String.length(full_name)
```

出力結果は次の通りです。

```
4
```

## 参考リンク

- [Elixir 公式サイト](https://elixir-lang.org/)
- [連結演算子 `<>` のドキュメント](https://hexdocs.pm/elixir/Kernel.html#<>/2)
- [IO.puts 関数のドキュメント](https://hexdocs.pm/elixir/IO.html#puts/1)