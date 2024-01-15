---
title:                "コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
html_title:           "Kotlin: コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
simple_title:         "コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ読書金コマンドライン引数に参加するのか

コマンドライン引数を読み込むことは、プログラミングの世界では非常に一般的な作業です。プログラマーはコマンドライン引数を使用して、プログラムの動作を制御したり、プログラムにデータを渡したりすることができます。そのため、コマンドライン引数の読み込みは必須のスキルです。

## 方法

まず、Kotlinの`args`配列でコマンドライン引数を受け取ります。例えば、以下のように書きます。

```Kotlin
fun main(args: Array<String>) {
    println("コマンドライン引数の数: ${args.size}")
    println("コマンドライン引数:")
    for (arg in args) {
        println(arg)
    }
}
```

このプログラムを実行すると、コマンドライン引数の数と内容が表示されます。例えば、コマンドラインで「kotlin main.kt test1 test2 test3」と入力した場合、以下のように表示されます。

```
コマンドライン引数の数: 3
コマンドライン引数:
test1
test2
test3
```

また、コマンドライン引数は文字列として受け取るため、必要に応じて`toInt()`や`toDouble()`などのメソッドを使用して数値に変換することもできます。

## ディープダイブ

コマンドライン引数を受け取る時、`args`配列のインデックスを指定することで特定の引数を取得することもできます。例えば、`args[0]`は最初の引数、`args[1]`は2番目の引数というように取得することができます。また、`args[0]`は`args.get(0)`と同じ意味になります。

さらに、Kotlinでは`args`配列以外にも`arg[0] to arg[1]`というような記法で引数を指定することもできます。ただし、`arg[0]`は最初の引数、`arg[1]`は2番目の引数というようにインデックスではなく位置で指定することに注意しましょう。

## また見てください

- [コマンドライン引数の詳細](https://kotlinlang.org/docs/command-line.html)
- [Kotlinの配列とリスト](https://kotlinlang.org/docs/basic-types.html#arrays)
- [Kotlinの日本語リファレンス](https://kotlinlang.org/docs/reference/)