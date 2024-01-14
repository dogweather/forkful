---
title:    "Kotlin: コンピュータ・プログラミングの記事：コマンドライン引数の読み込み"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ読むのか

コマンドライン引数を読むことは、Kotlinでプログラミングをする上で非常に便利です。コマンドライン引数を読むことにより、プログラムをより柔軟に設計することができ、ユーザーからの入力を受け付けることができます。

## コマンドライン引数を読む方法

コマンドライン引数を読むには、プログラムの実行時に渡される引数を取得する必要があります。これには、`main()`関数の引数として`args: Array<String>`を使用します。

例えば、次のようなコードを使用して引数を読むことができます。

```Kotlin
fun main(args: Array<String>) {
    println("コマンドライン引数の数は ${args.size} 個です。")
    args.forEachIndexed { index, arg ->
        println("引数 $index は $arg です。")
    }
}
```

このコードを実行した場合、以下のような出力が得られます。

```
コマンドライン引数の数は 3 個です。
引数 0 は arg1 です。
引数 1 は arg2 です。
引数 2 は arg3 です。
```

また、コマンドライン引数にはオプションを付けることもできます。例えば、`--verbose`や`-d`などのオプションを指定することができます。オプションは`main()`関数の引数として渡される配列の先頭に位置します。

```Kotlin
fun main(args: Array<String>) {
    val verbose = args[0] == "--verbose"
    println("オプション --verbose は $verbose です。")
}
```

このコードを実行した場合、以下のような出力が得られます。

```
オプション --verbose は true です。
```

## コマンドライン引数の詳細

コマンドライン引数には、様々な用途で使用することができます。例えば、プログラムの実行時に渡されたファイル名などを取得することができます。また、複数の値を取得することができるので、プログラムの実行後に実行する処理を変更することも可能です。

## 参考リンク

- [Kotlinプログラムでコマンドライン引数を受け取る方法](https://www.zoftino.com/command-line-arguments-in-kotlin)
- [Kotlinプログラムの実行時にコマンドライン引数を受け取る方法](https://www.tutorialkart.com/kotlin/kotlin-get-command-line-arguments/)
- [Kotlinプログラムでコマンドライン引数を取得する方法](https://kodejava.org/how-do-i-get-command-line-arguments-in-kotlin/)
- [Kotlinプログラムの実行時にオプション付きのコマンドライン引数を取得する方法](https://www.it-swarm-ja.tech/ja/kotlin/kotlin%E3%81%A7%E5%AE%9F%E8%A1%8C%E6%99%82%E3%81%AB%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E3%83%A9%E3%82%A4%E3%83%B3%E5%BC%95%E6%95%B0%E3%82%92%E5%8F%96%E5%BE%97%E3%81%99%E3%82%8B%E6%96%B9%E6%B3%95/1004109579/)

## 参考文