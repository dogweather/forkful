---
date: 2024-01-20 17:54:49.681459-07:00
description: "How to: \u7C21\u5358\u306B\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\
  \u65B9\u6CD5\u306F\u8272\u3005\u3042\u308A\u307E\u3059\u3002`readLines()` \u3092\
  \u4F7F\u3046\u3068\u4E00\u884C\u305A\u3064\u30EA\u30B9\u30C8\u3068\u3057\u3066\u8AAD\
  \u307F\u8FBC\u3081\u307E\u3059\u304C\u3001\u5927\u304D\u306A\u30D5\u30A1\u30A4\u30EB\
  \u306B\u306F\u9069\u3057\u3066\u3044\u307E\u305B\u3093\u3002\u305D\u306E\u5834\u5408\
  \u306B\u306F\u3001`bufferedReader()` \u3092\u4F7F\u3063\u3066\u30E1\u30E2\u30EA\u3092\
  \u7BC0\u7D04\u3057\u306A\u304C\u3089\u8AAD\u307F\u8FBC\u307F\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.968594-06:00'
model: gpt-4-1106-preview
summary: "\u7C21\u5358\u306B\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u65B9\u6CD5\
  \u306F\u8272\u3005\u3042\u308A\u307E\u3059\u3002`readLines()` \u3092\u4F7F\u3046\
  \u3068\u4E00\u884C\u305A\u3064\u30EA\u30B9\u30C8\u3068\u3057\u3066\u8AAD\u307F\u8FBC\
  \u3081\u307E\u3059\u304C\u3001\u5927\u304D\u306A\u30D5\u30A1\u30A4\u30EB\u306B\u306F\
  \u9069\u3057\u3066\u3044\u307E\u305B\u3093\u3002\u305D\u306E\u5834\u5408\u306B\u306F\
  \u3001`bufferedReader()` \u3092\u4F7F\u3063\u3066\u30E1\u30E2\u30EA\u3092\u7BC0\u7D04\
  \u3057\u306A\u304C\u3089\u8AAD\u307F\u8FBC\u307F\u307E\u3059\u3002"
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
