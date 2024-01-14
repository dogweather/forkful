---
title:    "Kotlin: 文字列の抽出"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ
文字列から部分文字列を抽出することの意義について説明します。

## 方法
以下に、Kotlinを使って文字列から部分文字列を抽出する方法を示します。
```Kotlin
// 文字列の定義
val str = "こんにちは、世界！"

// 先頭から5文字を抽出
val substr = str.substring(0, 5)
println(substr) // 出力：こんにちは

// 特定の文字列以降を抽出
val substr2 = str.substringAfter("、")
println(substr2) // 出力：世界！
```

## ディープダイブ
文字列から部分文字列を抽出する際の詳細な情報をご説明します。この方法を使用することで、プログラムの柔軟性を高めることができます。Kotlinでは、 `substring()` メソッドを使用することで、指定した範囲の文字列を抽出することができます。また、 `substringBefore()`、 `substringAfter()`、 `substringBeforeLast()`、 `substringAfterLast()` などのメソッドを使用することで、特定の文字列を基準に抽出することができます。これらのメソッドを組み合わせることで、より複雑な抽出も可能です。

## 他に読むべきもの
- [Java String substringメソッドの使用方法](https://www.javatpoint.com/java-string-substring)
- [Kotlin Strings](https://kotlinlang.org/docs/strings.html)