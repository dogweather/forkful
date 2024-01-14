---
title:                "Kotlin: テキストファイルを読む"
simple_title:         "テキストファイルを読む"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ読む必要があるのか？

テキストファイルとは、テキストデータを含むファイルのことです。コンピューターやスマートフォンなど、さまざまなデバイスで使用されています。テキストファイルを読むことは、プログラミングの世界で非常に重要です。例えば、データ処理やファイルの内容を表示するなど、さまざまな用途に使用されます。この記事では、Kotlinを使用してテキストファイルを読み込む方法を紹介します。

## 読み込み方法

まずは、Kotlinでテキストファイルを読み込む方法を学びましょう。下記のコードブロックを使用して、簡単なテキストファイルを読み込み、その内容をコンソールに表示することができます。

```Kotlin
fun main() {
    // ファイルを読み込む
    val file = File("テキストファイルのパス")
    // ファイルの内容を読み込む
    val content = file.readText()
    // ファイルの内容をコンソールに表示する
    println(content)
}
```

上記のように、まずはファイルを`File`クラスのインスタンスとして作成し、その後`readText()`メソッドを使用してファイルの中身を読み込みます。最後に、`println()`メソッドを使用してファイルの内容を表示します。

## 深堀り

テキストファイルを読み込む際に、より複雑な処理を行うこともできます。例えば、ファイルの中に特定の文字列が含まれているかどうかをチェックしたり、ファイルの内容を修正したりすることができます。以下のコードブロックは、特定の文字列が含まれている場合にその行を表示するプログラムの例です。

```Kotlin
fun main() {
    // ファイルを読み込む
    val file = File("テキストファイルのパス")
    // ファイルの内容を読み込む
    val content = file.readText()
    // ファイルの内容を行ごとに分割して配列に格納する
    val lines = content.split("\n")
    // 配列の中身をループさせる
    for (line in lines) {
        // もし特定の文字列が含まれていた場合は、その行を表示する
        if (line.contains("特定の文字列")) {
            println(line)
        }
    }
}
```

このように、Kotlinを使用することでテキストファイルを柔軟に操作することができます。

## 関連リンク

* Kotlin公式ドキュメント - https://kotlinlang.org/docs/reference/
* テキストファイルの読み込み方法まとめ - https://www.javadrive.jp/kotlin/file/index2.html
* Kotlinでのファイル操作のベストプラクティス - https://code-examples.net/ja/d/3095df