---
title:    "Kotlin: テキストファイルの作成"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

なぜ: テキストファイルを書くことに関わる理由

テキストファイルは、データを整理して保存するための重要なツールです。プログラミングにおいても、データを読み書きする必要があるため、テキストファイルを書くことは重要です。

この記事では、Kotlinを使ってテキストファイルを書く方法をご紹介します。

## テキストファイルを書く方法

テキストファイルを書くためには、まず必要なのはファイルを新規で作成することです。次に、ファイルに書き込む文字列を指定し、ファイルに書き込む処理を行います。

以下の例では、"Hello, world!"という文字列をカレントディレクトリに新しいファイルとして書き込みます。

```Kotlin
val str = "Hello, world!"
val file = File("output.txt")

file.writeText(str)
```

上記のコードを実行すると、"output.txt"というファイルが作成され、その中に"Hello, world!"という文字列が書き込まれます。

また、複数行の文字列を書き込む場合は、`writeText()`ではなく`writeLines()`を使います。

```Kotlin
val lines = listOf("This is line 1", "This is line 2", "This is line 3")
val file = File("output.txt")

file.writeLines(lines)
```

このように、`writeText()`や`writeLines()`メソッドを使うことで、簡単にテキストファイルを書くことができます。

## テキストファイルについての深掘り

テキストファイルは、コンピュータにとっては単なるバイトの列であるため、文字コードの設定が重要です。Kotlinでは、`File`クラスのインスタンスを作成する際に、文字コードを指定することができます。

また、ファイルの読み書きには`Charset`クラスを使用し、ファイルの文字コードを指定することもできます。

さらに、ファイルを開く際には`BufferedReader`や`BufferedWriter`クラスを使用することで、効率的に読み書きを行うことができます。

## それ以外にも参考になるリンク

- [Kotlin公式ドキュメント: ファイルの読み書き](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Kotlin Tips: テキストファイルの読み込みと書き込み](https://kotlinlang.org/docs/tutorials/kotlin-for-py/strings-and-regexes.html#stringio)
- [Tech Academy: KotlinのFileクラスでテキストファイルを扱う方法](https://techacademy.jp/magazine/20549)