---
aliases:
- /ja/kotlin/creating-a-temporary-file/
date: 2024-01-20 17:41:00.167248-07:00
description: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3068\u306F\u3001\u4E00\u6642\u7684\
  \u306B\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306E\u30D5\u30A1\
  \u30A4\u30EB\u306E\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\u5185\
  \u3067\u5927\u91CF\u306E\u30C7\u30FC\u30BF\u3092\u4E00\u6642\u7684\u306B\u6271\u3046\
  \u969B\u3084\u3001\u6C38\u7D9A\u7684\u306A\u4FDD\u5B58\u304C\u4E0D\u8981\u306A\u30C7\
  \u30FC\u30BF\u3092\u4F5C\u696D\u3059\u308B\u969B\u306B\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.897355
model: gpt-4-1106-preview
summary: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3068\u306F\u3001\u4E00\u6642\u7684\
  \u306B\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306E\u30D5\u30A1\
  \u30A4\u30EB\u306E\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\u5185\
  \u3067\u5927\u91CF\u306E\u30C7\u30FC\u30BF\u3092\u4E00\u6642\u7684\u306B\u6271\u3046\
  \u969B\u3084\u3001\u6C38\u7D9A\u7684\u306A\u4FDD\u5B58\u304C\u4E0D\u8981\u306A\u30C7\
  \u30FC\u30BF\u3092\u4F5C\u696D\u3059\u308B\u969B\u306B\u4F7F\u3044\u307E\u3059\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## What & Why?
一時ファイルとは、一時的にデータを保存するためのファイルのことです。プログラム内で大量のデータを一時的に扱う際や、永続的な保存が不要なデータを作業する際に使います。

## How to:
Kotlinでは、`createTempFile` 関数を使って簡単に一時ファイルを作成できます。以下はその使用例です。

```kotlin
import java.io.File

fun main() {
    // 一時ファイルの作成
    val tempFile: File = File.createTempFile("temp", ".txt")
    println("Temporary file created: ${tempFile.absolutePath}")

    // 一時ファイルへの書き込み
    tempFile.writeText("一時ファイルに保存されたデータ")

    // 一時ファイルの読み込み
    val text = tempFile.readText()
    println("Data read from temporary file: $text")

    // 一時ファイルの削除
    tempFile.deleteOnExit()
}
```

実行結果の出力は次のようになります。

```
Temporary file created: /tmp/temp1234567890.txt
Data read from temporary file: 一時ファイルに保存されたデータ
```

## Deep Dive
以前は一時ファイルを作成する際には、ファイル名を自分で生成して、その存在を確認する必要がありました。しかし、`java.io.File` クラスの導入により、一時ファイルの作成が容易になりました。

一時ファイルを使う代わりにメモリベースの解決策、例えば `ByteArrayOutputStream` を使用する方法もありますが、ファイルを使用するメリットは、大量のデータを扱う際にメモリを節約できる点です。

生成された一時ファイルはデフォルトでシステムの一時フォルダに保存されますが、第二、第三の引数でプレフィックスやサフィックスを指定することで名前をカスタマイズできます。また、`deleteOnExit` メソッドを使用することで、プログラム終了時に自動的にファイルを削除させることが可能です。

## See Also
- Kotlinの公式ドキュメント: [Fileのドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- `java.io.File` クラス: [Java Platform SEのドキュメント](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- メモリ効率についての詳細: [メモリ管理の基礎](https://developer.android.com/topic/performance/memory)
