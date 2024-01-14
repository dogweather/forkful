---
title:                "Kotlin: コンピュータープログラミングの記事のタイトル: コマンドライン引数の読み取り"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜ
プログラミングをしている人であれば、コマンドライン引数を読み込む必要があることがあります。例えば、プログラムを実行する際に、動作を変えるためのオプションを指定したり、外部から入力を受け取ったりする必要がある場合があります。

# ハウトゥー
コマンドライン引数を読み込む方法は非常に簡単です。まずは、`args`という配列を作成します。この配列には、プログラムが実行された際に与えられた引数が格納されます。次に、`args`配列を使用して、引数を読み込むことができます。

```
Kotlin
fun main(args: Array<String>) {
    // 引数を表示する例
    println(args[0])
}
```

もし、引数が複数ある場合は、インデックスを変えることで読み込むことができます。

```
Kotlin
fun main(args: Array<String>) {
    // 第二引数を表示する例
    println(args[1])
}
```

引数の数が可変の場合は、`for`ループを使用して全ての引数を取得することができます。

```
Kotlin
fun main(args: Array<String>) {
    // 全ての引数を表示する例
    for (i in 0 until args.size) {
        println(args[i])
    } 
}
```

上記のコードを実行すると、プログラムが実行された際に与えられた全ての引数が表示されます。

# ディープダイブ
コマンドライン引数を使用する際に注意しなければならない点がいくつかあります。まず、引数が与えられなかった場合は、`args`配列のサイズは0になります。そのため、必ずサイズをチェックしてから引数を読み込むようにしましょう。

次に、引数には文字列以外の型も入っている可能性があります。その場合は、`toInt()`や`toBoolean()`などの変換メソッドを使用して型を変換する必要があります。

また、コマンドライン引数にはオプションを指定することもできます。例えば、`--verbose`というオプションを指定することで、実行時に詳細なログを表示することができます。このようなオプションを取得するには、`startsWith()`メソッドを使用することで実現できます。

さらに、コマンドライン引数を扱うためのライブラリやフレームワークもあります。これらを使用することで、さらに柔軟で簡潔なコードを書くことができるようになります。

# その他参考になるリンク
- [Kotlinプログラミング言語公式サイト](https://kotlinlang.org/docs/reference/basic-syntax.html#command-line-arguments)
- [Kotlinの「args」の意味と使い方](https://maku77.github.io/kotlin/basic/args.html)
- [Kotlinの配列・リストをループして要素を取り出す](https://qiita.com/LeoAndo/items/af735d0a6ccc44e03e4a)
- [Apache Commons CLI](https://github.com/apache/commons-cli)（コマンドライン引数を扱うためのライブラリ）
- [JCommander](https://github.com