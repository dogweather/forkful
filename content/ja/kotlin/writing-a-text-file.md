---
title:    "Kotlin: テキストファイルの作成"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜテキストファイルを作成するのか

テキストファイルを作成することで、プログラムの出力やデータの保存など、さまざまな目的を達成することができます。

## 作成方法

テキストファイルを作成するには、Kotlinの```File```クラスを使用します。以下のコードを使用して、新しいテキストファイルを作成し、テキストを書き込むことができます。

```kotlin
val file = File("sample.txt")
file.writeText("Hello, world!")
```

上記のコードでは、まず```File```クラスを使用して新しいファイルを作成し、そのファイルに```writeText()```メソッドを使用してテキストを書き込んでいます。ここで、ファイル名は```sample.txt```として設定されていますが、任意のファイル名を使用することができます。

## ディープダイブ

テキストファイルを作成する際には、ファイルのパスや書き込むテキストのエンコーディングを指定することができます。また、ファイルを開く際には```FileWriter```クラスを使用することもできます。さらに、テキストファイルを読み込む際には```BufferedReader```クラスを使用できます。

## その他にも参考になるリンク

[Fileクラスのドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)  
[Kotlinのファイル操作についてのチュートリアル](https://www.tutorialsbuddy.com/kotlin-file-handling)  
[Kotlinでファイルを読み書きする方法](https://www.geeksforgeeks.org/kotlin-file-inputoutput-operations/)