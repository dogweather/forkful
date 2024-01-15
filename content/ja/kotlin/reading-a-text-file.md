---
title:                "「テキストファイルの読み込み」"
html_title:           "Kotlin: 「テキストファイルの読み込み」"
simple_title:         "「テキストファイルの読み込み」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

なぜテキストファイルを読むことに興味を持つのか？それは、テキストファイルがコンピューターやプログラミングにおいて欠かせないものであるからです。テキストファイルは、様々なデータを格納し、処理するための重要な手段です。

## How To

テキストファイルを読み込むには、Kotlinで以下のようにコードを書けばいいです。

```
Kotlin
val file = File("text.txt")
file.forEachLine {
    println(it)
}
```

これは、ファイルの各行を読み込み、コンソールに出力するコードです。もちろん、ファイルの内容を実際に処理することも可能です。

```
Kotlin
val file = File("names.txt")
val names = mutableListOf<String>()

file.forEachLine {
    names.add(it)
}
println("読み込まれた名前の数は ${names.size} です。")
```

上記のコードは、テキストファイルに記録された名前をリストに追加し、最後にその数を出力するものです。

## Deep Dive

テキストファイルを読み込む際には、ファイルのエンコーディングに気をつける必要があります。特に日本語のようなマルチバイト文字を含む場合は、適切なエンコーディングを指定する必要があります。Kotlinでは、`File()`メソッドの引数にエンコーディングを指定することで、問題なくファイルを読み込むことができます。

また、大規模なテキストファイルを読み込む際には、メモリの消費にも注意が必要です。その場合、`readLines()`メソッドを使用し、必要な行だけを読み込むことができます。

## See Also

- [Kotlin File Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Reading and Writing Files in Kotlin](https://medium.com/@thomashambach/reading-and-writing-files-in-kotlin-a73ab397acc6)