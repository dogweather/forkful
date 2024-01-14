---
title:                "Kotlin: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することについて、なぜ誰かが取り組むのか、理由を説明します。

## 方法

文字列を小文字に変換するには、`toLowerCase()`メソッドを使用します。例えば、以下のようにコーディングします。

```Kotlin
val str = "Hello World"
println(str.toLowerCase())
```

これにより、出力は次のようになります。

```
hello world
```

## ディープダイブ

`toLowerCase()`メソッドは、与えられた文字列を小文字に変換するための便利なメソッドです。一般的に、文字列を比較する際に、大文字と小文字を区別したくない場合にこのメソッドを使用します。

また、Kotlinでは`toLowerCase()`メソッドの他に、`uppercase()`メソッドも提供されています。これは文字列をすべて大文字に変換してくれます。

## 詳細を見る

Kotlinでは、文字列を小文字に変換するための他にも便利なメソッドや関数が提供されています。例えば、`trim()`メソッドを使用することで文字列の前後の空白を取り除くことができます。

また、文字列を操作する際には、`StringBuilder()`クラスを使用することでパフォーマンスを向上させることができます。

## 関連リンクを見る

[公式ドキュメント](https://kotlinlang.org/docs/strings.html#string-case-manipulation-methods)
[Kotlinで文字列を比較する方法](https://www.geeksforgeeks.org/how-to-compare-two-string-in-kotlin/)
[文字列操作についての詳細なガイド](https://dev.to/quickbirdstudios/kotlin-string-the-8-most-common-operations-and-classes-you-can-use-51b2)