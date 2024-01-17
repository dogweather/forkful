---
title:                "文字列を小文字に変換する"
html_title:           "Kotlin: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

文字列を小文字に変換することは、プログラマーにとって非常に一般的なタスクです。これは、入力された文字列の大文字や小文字が異なっている場合、正しく動作しない可能性があるためです。そのため、文字列を小文字に統一することで、不必要なバグを防ぐことができます。

## 方法：

```Kotlin
val text = "Hello, World!"
println(text.toLowerCase())

// 出力：hello, world!
```

上記のように、Kotlinでは```toLowerCase()```関数を使用することで、文字列を小文字に変換することができます。

また、```toLowerCase()```関数は日本語を含むマルチバイト文字列にも対応しているため、安心して使用することができます。

## 深堀り：

Javaでは、文字列の小文字変換には```toLowerCase()```メソッドもしくは```CaseFormat.LOWER_CASE.toConverter()```メソッドが使用されていましたが、Kotlinではより簡潔かつ使いやすい```toLowerCase()```関数が導入されました。

これ以外にも、正規表現を使って文字列を小文字に変換する方法や、文字コードを変換する方法などもありますが、Kotlinの```toLowerCase()```関数が最も一般的で推奨される方法です。

## 関連情報：

- [Kotlinドキュメンテーション](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lowercase.html)
- [Java Stringの小文字変換メソッド](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Java CaseFormatの説明](https://github.com/google/guava/wiki/StringsExplained#caseformat)