---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

一時ファイルとは、プログラムが一時的にデータを保存するためのファイルです。テストデータの保存や一時的なデータの管理など、ディスクとメモリ間の情報のやり取りに使用されます。

## 使い方：

Kotlinを使って一時ファイルを作るための典型的な方法は以下の通りです。

```Kotlin
import java.io.File

fun main() {
    val tempFile = createTempFile("temp", ".txt").also {
        it.writeText("ここにテストデータを書き込み")
    }

    val readText = tempFile.readText()
    println(readText) // "ここにテストデータを書き込み"が出力されます。

    tempFile.delete() // ファイルを削除します。
}
```

## 詳細：

**歴史的背景**：一時ファイルは古くから存在し、初期のOSから使用されています。これにより、プログラムは非常に大きなデータを安全に扱うことができます。

**代替手段**：メモリ内ストリームを使用すると、ディスクへのアクセスを避けることができます。ただし、これは大量のデータを扱うことができないという制限があります。

**実装詳細**：Kotlinでは、`createTempFile`メソッドを使用して一時ファイルを生成します。このメソッドは一時ファイルをシステムの一時ディレクトリに作成します。

## 参考資料：

* [Kotlin公式ドキュメンテーション](https://kotlinlang.org/docs/reference/io-files.html)
* [Javaの一時ファイル](https://www.baeldung.com/java-create-temporary-file)