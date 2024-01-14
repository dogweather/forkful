---
title:                "Java: 文字列の連結"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
文字列を連結することについて、なぜその方法を採用するのかを説明します。

## メソッド
文字列を連結する方法はいくつかありますが、基本的な方法としては「+」演算子を使用する方法があります。例えば、次のようなコードを使用することで、2つの文字列を連結することができます。

```java
String str1 = "こんばんは";
String str2 = "。";
String result = str1 + str2;
System.out.println(result); // 出力結果: こんばんは。
```

もう1つの方法として、```StringBuffer```クラスを使用する方法があります。これは、可変の文字列を扱うことができるクラスで、連結するためのメソッドとして```append()```があります。例えば、次のようなコードを使用することで、同じように2つの文字列を連結することができます。

```java
String str1 = "こんにちは";
String str2 = "！";
StringBuffer sb = new StringBuffer();
sb.append(str1);
sb.append(str2);
System.out.println(sb.toString()); // 出力結果: こんにちは！
```

## 詳細について
文字列の連結方法には、メモリの使用方法やパフォーマンスに関わる重要な点があります。例えば、上記の2つの方法の場合、```String```クラスを使用する方法の方がメモリの使用量が大きくなり、その結果パフォーマンスの面で劣ることがあります。そのため、大量の文字列を連結する場合は、```StringBuffer```クラスを使用する方が良いでしょう。

また、連結する文字列の数が少ない場合は、```String```クラスを使用した方がシンプルで理解しやすいコードになると言えます。

## 参考リンク
- [Java String Concatenation](https://www.baeldung.com/java-string-concatenation)
- [StringBuffer Class in Java](https://www.geeksforgeeks.org/stringbuffer-class-in-java/)
- [String vs StringBuffer in java](https://www.guru99.com/string-vs-stringbuffer-vs-stringbuilder.html)

## 関連情報
- Javaでは、文字列を連結する方法以外にも、文字列を扱うためのさまざまなメソッドやクラスが用意されています。ぜひ、それらも学習してみてください。