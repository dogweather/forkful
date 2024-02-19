---
aliases:
- /ja/kotlin/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:39.200224-07:00
description: "Kotlin\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\
  \u304F\u3053\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3057\u3066\
  \u30C6\u30AD\u30B9\u30C8\u5185\u5BB9\u3092\u5165\u529B\u3059\u308B\u4F5C\u696D\u3067\
  \u3042\u308A\u3001\u30C7\u30FC\u30BF\u306E\u4FDD\u5B58\u3001\u30ED\u30B0\u8A18\u9332\
  \u3001\u307E\u305F\u306F\u8A2D\u5B9A\u306E\u8A2D\u5B9A\u306A\u3069\u306E\u4E00\u822C\
  \u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u63EE\u767A\u6027\u30E1\u30E2\u30EA\u7A7A\u9593\u306E\u5916\u3067\u30C7\
  \u30FC\u30BF\u3092\u4FDD\u5B58\u304A\u3088\u3073\u64CD\u4F5C\u3057\u3001\u30BB\u30C3\
  \u30B7\u30E7\u30F3\u9593\u3067\u306E\u6C38\u7D9A\u6027\u3092\u78BA\u4FDD\u3059\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.896332
model: gpt-4-0125-preview
summary: "Kotlin\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\
  \u304F\u3053\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3057\u3066\
  \u30C6\u30AD\u30B9\u30C8\u5185\u5BB9\u3092\u5165\u529B\u3059\u308B\u4F5C\u696D\u3067\
  \u3042\u308A\u3001\u30C7\u30FC\u30BF\u306E\u4FDD\u5B58\u3001\u30ED\u30B0\u8A18\u9332\
  \u3001\u307E\u305F\u306F\u8A2D\u5B9A\u306E\u8A2D\u5B9A\u306A\u3069\u306E\u4E00\u822C\
  \u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u63EE\u767A\u6027\u30E1\u30E2\u30EA\u7A7A\u9593\u306E\u5916\u3067\u30C7\
  \u30FC\u30BF\u3092\u4FDD\u5B58\u304A\u3088\u3073\u64CD\u4F5C\u3057\u3001\u30BB\u30C3\
  \u30B7\u30E7\u30F3\u9593\u3067\u306E\u6C38\u7D9A\u6027\u3092\u78BA\u4FDD\u3059\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？
Kotlinでテキストファイルを書くことは、ファイルを作成してテキスト内容を入力する作業であり、データの保存、ログ記録、または設定の設定などの一般的なタスクです。プログラマーは、揮発性メモリ空間の外でデータを保存および操作し、セッション間での永続性を確保するためにこれを行います。

## 方法：
Kotlinは、追加のサードパーティ製ライブラリなしで標準ライブラリを活用することで、ファイルへの書き込みに対して直感的なアプローチを提供します。ここに簡単な例があります：

```kotlin
import java.io.File

fun main() {
    val textToWrite = "Hello, Kotlin file writing!"
    File("example.txt").writeText(textToWrite)
}
```
このコードスニペットは、「example.txt」という名前のファイルをプロジェクトのルートディレクトリに作成し、その中に `Hello, Kotlin file writing!` という文字列を書き込みます。ファイルがすでに存在する場合は、上書きされます。

ファイルに対してより制御された追記を行ったり、大量のデータを書き込んだりする場合は、`appendText`や`bufferedWriter()`を使用できます：

```kotlin
import java.io.File

fun appendToFile() {
    val moreText = "Appending more text."
    File("example.txt").appendText(moreText)
}

fun writeWithBufferedWriter() {
    val largeText = "Large amounts of text...\nOn multiple lines."
    File("output.txt").bufferedWriter().use { out ->
        out.write(largeText)
    }
}

fun main() {
    appendToFile() // 既存のファイルにテキストを追記する
    writeWithBufferedWriter() // 大量のテキストデータを効率的に書き込む
}
```

`appendToFile`関数では、"example.txt"の現在の内容を上書きすることなく、さらにテキストを追加しています。`writeWithBufferedWriter`関数は、特に複数の行や大きなファイルを扱う際にI/O操作を最小限に抑えるのに役立つ、大量のテキストやデータを効率的に書き込む方法を示しています。

これらの例は、Kotlinでテキストファイルを書くための基本操作をカバーしており、ファイルI/O操作に対するKotlinの標準ライブラリの簡単さとパワーを示しています。
