---
title:    "Kotlin: 文字列の大文字化"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## なぜ
文字列の大文字化を行う理由は、飽くまでプログラミングのスキルを向上させるためです。正確には、文字列の処理や操作をより効率的に行うためには、文字列の大文字化という基本的な機能を理解する必要があります。

## 方法
次のように `toUpperCase()` メソッドを使用することで、文字列を簡単に大文字化することができます。

```Kotlin
val str = "Hello, World!"
val strNew = str.toUpperCase()
println(strNew)
```

出力：
`HELLO, WORLD!`

`toUpperCase()` メソッドは、元の文字列を変更せずに新しい大文字化された文字列を返すことに注意してください。

## 深堀り
文字列の大文字化には、さまざまな方法があります。例えば、ASCII文字列の場合、文字の値を変更して大文字化を行う方法があります。また、正規表現を使用して特定の文字列パターンを検索し、それを大文字化することもできます。さらに、Kotlinの `toUpperCase()` メソッドは、標準のUnicode変換を使用するため、多言語対応が可能です。

## 関連記事
- [Kotlinドキュメンテーション](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)
- [ASCIIコードを使用した文字列の大文字化方法](https://www.geeksforgeeks.org/convert-string-uppercase/)
- [正規表現を使用した文字列パターンの検索と大文字化](https://www.guru99.com/string-regex.html)

## 他に見る
- [Kotlinの文字列操作方法](https://www.tutorialspoint.com/kotlin/kotlin_strings.htm)
- [標準のUnicode変換について](https://en.wikipedia.org/wiki/Unicode_equivalence)