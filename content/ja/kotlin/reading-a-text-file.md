---
title:                "テキストファイルの読み込み"
html_title:           "Kotlin: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なに & なぜ？

テキストファイルを読むこととは、コンピューターに保存されているテキストをプログラムで読み取ることを意味します。プログラマーがテキストファイルを読む理由は、そのファイルに保存されているデータを処理し、プログラムの機能を向上させるためです。

## やり方：

テキストファイルを読む方法は簡単です。まず、ファイルを変数に割り当て、ファイルからデータを読み取ります。コードの一例をご紹介します。

```Kotlin
// ファイルを変数に割り当てる
val file = File("example.txt")

// ファイルからデータを読み取る
file.forEachLine { line ->
    println(line) // 読み取ったデータを表示
}
```

上記のコードは、ファイルを1行ずつ読み取って、読み取ったデータを表示するものです。もちろん、実際のプログラムではさらに処理を行うことが可能です。

## 深堀り：

テキストファイルを読む方法にはいくつかのオプションがあります。上記の例では、`forEachLine`メソッドを使用しましたが、`readLines`メソッドや`bufferedReader`クラスを使用することもできます。また、テキストファイルのエンコーディングを指定することで、日本語のようなマルチバイト文字を正しく読み取ることが可能です。

また、テキストファイルの他にも、バイナリファイルを読み取る方法もあります。バイナリファイルを読み取る際には、`inputStream`クラスを使用します。

## 関連リンク：

- Kotlinの公式ドキュメント：https://kotlinlang.org/
- ファイルの読み書きについての記事：https://qiita.com/kyota/items/dc022650c708b6b31115