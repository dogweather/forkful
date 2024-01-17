---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "Kotlin: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

ディレクトリの存在をチェックすることは、プログラマーが特定のディレクトリが存在するかどうかを確認することです。プログラマーは、コンピューター上で必要なファイルやフォルダーが存在するかどうかを確認するために、これを行う必要があります。

## 方法：

```Kotlin
val file = File("testDirectory")
if (file.exists()) {
    println("testDirectory already exists.")
} else {
    println("testDirectory does not exist.")
}
```

出力：

```
testDirectory does not exist.
```

## 深堀り：

ディレクトリの存在をチェックすることは、コンピューターのファイルシステムを理解する上でとても重要です。適切なファイルやフォルダーが存在しない場合、プログラムはエラーを引き起こす可能性があります。このチェックを行う代替手段としては、例外をキャッチしたり、ファイルのパスを変更したりすることがあります。また、この操作はプログラムのパフォーマンスにも影響することがあり、複数のディレクトリをチェックする必要がある場合は、より最適なアルゴリズムを使用することが重要です。

## 参考：

- [Kotlin 公式ドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html)
- [Java World: Checking File Existence](https://www.javaworld.com/article/2077707/checking-file-existence.html)