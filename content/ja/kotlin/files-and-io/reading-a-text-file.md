---
date: 2024-01-20 17:54:49.681459-07:00
description: 'How to: .'
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.089597-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

## How to:
```kotlin
import java.io.File

fun readFileAsLines(fileName: String): List<String> = File(fileName).readLines()

fun main() {
    val lines = readFileAsLines("example.txt")
    lines.forEach { println(it) }
}
```
サンプル出力:
```
最初の行です
次の行です
さらに、もう一行です
```

## Deep Dive
簡単にファイルを読む方法は色々あります。`readLines()` を使うと一行ずつリストとして読み込めますが、大きなファイルには適していません。その場合には、`bufferedReader()` を使ってメモリを節約しながら読み込みます。

```kotlin
fun readFileWithBufferedReader(fileName: String) {
    File(fileName).bufferedReader().use { reader ->
        var line = reader.readLine()
        while (line != null) {
            println(line)
            line = reader.readLine()
        }
    }
}
```

古い方法には `FileInputStream` や `Scanner` がありますが、Kotlinではもっと読みやすい関数が用意されています。

ファイルの読み込みではエラー処理も重要です。例えば、ファイルが見つからない、読み取り権限がない等の状況では、適切にエラーを処理するコードが必要です。

## See Also
- Kotlin公式ドキュメント: [Reading and writing files](https://kotlinlang.org/docs/idioms.html#read-a-file)
- Java公式ガイド: [BufferedReader](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- Kotlinによるファイル読み込みのさらに多くの例: [Kotlin's use-site variance](https://kotlinlang.org/docs/reference/generics.html#use-site-variance-type-projections)
