---
date: 2024-01-20 17:54:49.681459-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\
  \u307F\u3068\u306F\u3001\u30C7\u30FC\u30BF\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u5185\
  \u3067\u4F7F\u3048\u308B\u5F62\u306B\u5909\u63DB\u3059\u308B\u30D7\u30ED\u30BB\u30B9\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u3001\u30C7\
  \u30FC\u30BF\u306E\u30A4\u30F3\u30DD\u30FC\u30C8\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u306E\u5165\u529B\u306A\u3069\u306E\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.089597-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\
  \u307F\u3068\u306F\u3001\u30C7\u30FC\u30BF\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u5185\
  \u3067\u4F7F\u3048\u308B\u5F62\u306B\u5909\u63DB\u3059\u308B\u30D7\u30ED\u30BB\u30B9\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u3001\u30C7\
  \u30FC\u30BF\u306E\u30A4\u30F3\u30DD\u30FC\u30C8\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u306E\u5165\u529B\u306A\u3069\u306E\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

## What & Why?
テキストファイルの読み込みとは、データをプログラム内で使える形に変換するプロセスです。プログラマーは設定、データのインポート、アプリケーションの入力などのためにこれを行います。

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
