---
title:    "Elixir: 文字列の大文字化"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ？

文字列の大文字化を行うことの利点や重要性を知りたいですか？Elixirでの文字列処理は非常に便利です。大文字化は、データの整理や、見やすくするために活用されることがあります。

## 方法

まずは、ElixirのString.capitalize/1関数を使い、大文字化したい文字列を引数として渡します。例えば、"elixir"という文字列を渡すと、"Elixir"という出力が得られます。

```Elixir
String.capitalize("elixir")
> "Elixir"
```

また、String.upcase/1関数を使うことで、すべての文字を大文字にすることができます。例えば、"hello"という文字列を渡すと、"HELLO"という出力が得られます。

```Elixir
String.upcase("hello")
> "HELLO"
```

## ディープダイブ

Elixirでは、String.capitalize/1やString.upcase/1のような文字列処理を行う関数が多く提供されています。これらの関数を組み合わせることで、柔軟な文字列処理が可能です。

また、Elixirでは文字列だけでなく、リストやバイナリなど様々なデータ型の処理も行うことができます。さらにパターンマッチングや関数のパイプラインなどの機能を使うことで、より効率的なコードを書くことができます。

## 参考リンク

- [Elixir 公式ドキュメント](https://elixir-lang.org/getting-started/basic-types.html#strings)
- [Elixir School - Strings](https://elixirschool.com/en/lessons/basics/basics/#strings)
- [Elixirで楽しく文字列を処理しよう](https://qiita.com/hikiko_/items/8a9df2ef599014ffd848) 

##参考資料