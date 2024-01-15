---
title:                "ディレクトリが存在するかどうかの確認。"
html_title:           "Kotlin: ディレクトリが存在するかどうかの確認。"
simple_title:         "ディレクトリが存在するかどうかの確認。"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することの利点は、アプリケーションが必要なファイルやリソースを正しく読み込むために重要です。ディレクトリが存在しない場合、プログラムは予期しないエラーを引き起こし、アプリケーションの動作に影響を及ぼす可能性があります。

## 方法

```Kotlin
fun checkDirectory(path: String): Boolean {
    val directory = File(path)
    return directory.exists()
}

fun main() {
    val path = "data/images"
    val directoryExists = checkDirectory(path)
    println("Does $path exist? $directoryExists")
}
```
実行結果:
```
Does data/images exist? true
```

## ディープダイブ

Kotlinの標準ライブラリには、ファイルやディレクトリのようなオペレーティングシステムの特定のリソースにアクセスするための便利なメソッドが用意されています。その中には、ディレクトリの存在を確認するための `exists()` メソッドも含まれています。このメソッドは、該当するファイルやディレクトリが存在する場合に `true` を返し、存在しない場合には `false` を返します。また、`exists()` メソッドの他にも、ディレクトリを作成するための `mkdir()` メソッドや削除するための `delete()` メソッドなど、便利なメソッドが数多くあります。

## 関連リンク

- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/)
- [Kotlinでディレクトリを作成する方法](https://www.developer.com/lang/how-to-create-directory-in-kotlin.html)
- [Kotlinでファイルやディレクトリを削除する方法](https://www.tutorialkart.com/kotlin/kotlin-delete-file-and-directory/)