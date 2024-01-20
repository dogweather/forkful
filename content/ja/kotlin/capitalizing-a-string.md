---
title:                "文字列を大文字にする"
html_title:           "Kotlin: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
大文字化は、文字列の最初の文字を大文字に変換するプログラミング技術です。わかりやすく、一貫性を持たせるために、プログラマーがよく利用します。

## 使い方：
以下に、`capitalize()` メソッドを使って文字列を大文字化するKotlinのコード例を示します。

```Kotlin
fun main() {
    val myString = "hello, world!"
    println(myString.capitalize())
}
```
出力：
```
Hello, world!
```
この例では、"hello, world!"という文字列の最初の文字'h'が大文字の'H'に変換されます。

## 深掘り:
大文字化は初期のプログラミング言語から存在し、ユーザーがテキストを一貫性を持って読むことを促すための一般的なプラクティスです。たとえば、名前、タイトル、見出しなど。

代替手段として、`toUpperCase()` を使用して文字列全体を大文字にすることもできます。ただし、これは文字列全体、すべての文字を大文字に変換しますので注意してください。

```Kotlin
fun main() {
    val myString = "hello, world!"
    println(myString.toUpperCase())
}
```
出力：
```
HELLO, WORLD!
```
大文字化の実装について言えば、Kotlinでは `capitalize()` メソッドが内部的に使用されています。このメソッドはStringクラスの一部であり、文字列の最初の文字を大文字に変換し、残りの文字列をそのまま返します。

## 参照：
1. Kotlin 文字列の公式ドキュメンテーション: [リンク](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
2. `capitalize()`、 `toUpperCase()` などの方法の詳細: [リンク](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)