---
title:                "Kotlin: 一時ファイルの作成"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ？

一時ファイルを作成するのには、いくつかの理由があります。最も一般的な理由は、一時ファイルを使用してプログラムを実行する際に、一時的なデータを保存することです。また、一時ファイルはシステムリソースの消費を少なくし、パフォーマンスを向上させることもできます。

## 作り方

まず、ファイルを作成するために必要な権限を確認します。次に、`createTempFile()`メソッドを使用して、一時ファイルを作成します。以下の例をご覧ください。

```Kotlin
val tempFile = createTempFile("prefix", "txt")
println(tempFile.absolutePath)
println(tempFile.delete())
```
出力結果:
```
/Users/username/prefix123456.txt
true
```

`createTempFile()`メソッドには、prefix（ファイル名の接頭辞）とsuffix（ファイル名の拡張子）の2つのオプションの引数があります。さらに、デフォルトでは、一時ファイルはシステムの一時フォルダーに作成されます。ファイルが不要になったら、`delete()`メソッドを使用して、ファイルを削除することができます。

## 詳しく見る

`createTempFile()`メソッドは、内部的にはJavaの`File.createTempFile()`メソッドを使用しています。そのため、一時ファイルの作成に関するさらに詳しい情報は、Javaのドキュメントを参照することができます。また、一時ファイルを使用する際の注意点として、ファイルが削除されずに残ってしまうことがある点についても知っておく必要があります。

## また見る

- [Kotlinドキュメント - `createTempFile()`メソッド](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Javaドキュメント - `File.createTempFile()`メソッド](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String,%20java.io.File))
- [Java Code Geeks - Understanding Java's Temporary Files](https://www.javacodegeeks.com/2016/01/understanding-javas-temporary-files.html)