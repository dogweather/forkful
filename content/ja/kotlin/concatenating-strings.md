---
title:    "Kotlin: 文字列の連結"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## なぜ

文字列を連結することの利点は、複数の部分から文字列を作成する際に便利で効率的な方法です。例えば、名前と姓の両方を含むフルネームを作る場合に、それぞれの文字列を別々に処理するよりも、一度に結合する方法があります。

## 方法

```
/** 
* 2つの文字列を連結する例
*/
fun main() { 
  val firstName = "太郎" 
  val lastName = "山田" 
  val fullName = firstName + " " + lastName 
  println(fullName) 
}

// 出力： 太郎 山田 
``` 
```
/** 
* 文字列と数値を連結する例
*/
fun main() { 
  val name = "太郎" 
  val age = 25 
  var message = "$name は $age 歳です" 
  println(message) 
}

// 出力： 太郎 は 25 歳です
```

## ディープダイブ

Kotlinでは、文字列を結合する方法として、`+`演算子と文字列テンプレートの2つがあります。`+`演算子を使用すると、右側の文字列が左側の文字列の後ろに結合されます。一方、文字列テンプレートを使用すると、変数や式を含む文字列をより簡単に作成できます。また、Kotlinでは、文字列を長く結合する場合にはStringBuilderを使用することが推奨されています。文字列テンプレートと同様に、変数や式を含む文字列を作成できるだけでなく、文字列の結合に対する処理速度も向上させることができます。

## 参考サイト

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Concatenating Strings in Kotlin](https://www.baeldung.com/kotlin/concatenate-strings)
- [Kotlin String Templates and Simple Template Expression Examples](https://attacomsian.com/blog/kotlin-string-template-and-simple-template-expression-examples)
- [StringBuilder class in Kotlin](https://www.geeksforgeeks.org/stringbuilder-class-kotlin/)