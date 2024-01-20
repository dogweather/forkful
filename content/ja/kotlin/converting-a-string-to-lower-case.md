---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換するとは、文字列内のすべての大文字を対応する小文字に変換することを指します。プログラマーはデータの一貫性を保つためや、大文字や小文字の違いによるエラーを避けるために、この技術を使用します。

## やり方：

Kotlinで文字列を小文字に変換する方法を以下に示します。

```Kotlin
fun main() {
    val myString = "HELLO WORLD"
    val lowerCaseString = myString.toLowerCase()

    println(lowerCaseString)
}
```

上記のコードの出力：

```Bash
hello world
```

## 深堀り

1. 歴史的文脈: 伝統的なプログラミングで、小文字への変換は常に一貫性とデータクリーニングを実現するための実証済みの戦略でした。今日、最新の言語であるKotlinでは、小文字への変換は簡単なメソッド呼び出しで行えます。

2. 代替手段: 文字列操作には他の方法もあります。大文字化（toUpperCase）、文字列のトリミング（trim）、文字列の分割（split）などが含まれます。

3. 実装詳細: `toLowerCase`メソッドは、文字列内のすべてのキャラクターをASCIIコードに変換し、その小文字バージョンを吐き出します。このプロセスは非常に効率的で高速です。

## 関連リンク


2. [toUpperCaseメソッドについて](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)

3. [Kotlinでのtrimメソッド使用](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/trim.html)

4. [Kotlinでの分割メソッド](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/split.html)