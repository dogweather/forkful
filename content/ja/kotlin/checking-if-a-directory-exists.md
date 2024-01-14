---
title:                "Kotlin: ディレクトリが存在するかどうかを確認する"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックする理由は、ファイルやデータを読み込む前に、その存在を確認する必要があるからです。

## 方法

ディレクトリが存在するかどうかをチェックするには、Kotlinの`File`クラスを使用します。以下のコードブロックを参考にしてください。 

```Kotlin
val directory = File("path/to/directory")
if(directory.exists()) {
    println("Directory exists!")
} else {
    println("Directory does not exist!")
}
```

上記のコードでは、`exists()`メソッドを使用してディレクトリの存在をチェックしています。もしディレクトリが存在すれば、`Directory exists!`というメッセージが表示され、存在しなければ`Directory does not exist!`というメッセージが表示されます。

## 深堀り

ディレクトリの存在をチェックする方法には、さまざまなアプローチがあります。例えば、`File`クラスの`isDirectory()`メソッドを使用することもできます。これは、指定されたファイルがディレクトリであるかどうかをチェックするものです。また、`listFiles()`メソッドを使用することで、指定されたディレクトリ内のファイルやディレクトリのリストを取得することもできます。

## See Also

- [Kotlin File Class Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [How to check if a file or directory exists in Kotlin](https://www.techiedelight.com/check-file-directory-exists-kotlin/)
- [Kotlin - Check if Directory Exists - GeeksforGeeks](https://www.geeksforgeeks.org/kotlin-check-if-directory-exists/)