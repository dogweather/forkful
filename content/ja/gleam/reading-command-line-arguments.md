---
title:    "Gleam: コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

Gleamのコマンドライン引数を読み込むのにトピックについて説明します。

## なぜ

コマンドライン引数を読み込むことで、Gleamプログラムに柔軟性を与えることができます。プログラムの挙動を実行時にユーザーが設定できるようになります。

## 方法

まず最初に、コマンドライン引数を格納する変数を定義しましょう。

```Gleam
let args = Sys.args()
```

この変数には、実行時に与えられた全ての引数が文字列のリストとして格納されます。

次に、このリストをループして引数を1つずつ処理することができます。例えば、引数の数を出力するプログラムを書いてみましょう。

```Gleam
import gleam/io

for arg in args {
  io.print(arg)
}

```

このプログラムを実行すると、引数の数が画面上に表示されます。

```bash
$ gleam run args.gleam hello world
2
```

## 深堀り

コマンドライン引数は、`Sys.args()`を使用してアクセスできますが、この関数にはオプション引数を指定することもできます。例えば、`Sys.args("option")`とすると、オプション引数のみを受け取ることができます。

また、コマンドライン引数にはパターンマッチングを使用することもできます。例えば、引数が整数の場合にはそれを数値として処理し、文字列の場合にはエラーを返すようなプログラムを書くことができます。

```Gleam
import gleam/io
import gleam/string

for arg in args {
  case string.to_int(arg) {
    Ok(num) -> io.print("This is the number: " ++ num)
    Error(_e) -> io.print("This is not a number")
  }
}
```

## See Also

- [Gleamの入門](https://gleam.run/book/introduction.html)
- [Gleamの構文ガイド](https://gleam.run/book/syntax.html)
- [Gleamの標準ライブラリ](https://gleam.run/doc/stdlib.html)