---
title:    "Kotlin: 文字列の長さを見つける"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# なぜ文字列の長さを求めるのか

プログラミングをする際には、文字列の長さを知る必要があることがあります。例えば、ユーザーが入力した文字列が一定の長さを超えたら特定の処理を行うようにするなど、プログラムの制御において重要な情報です。

## 方法

Kotlinでは、stringの `length` プロパティを用いて文字列の長さを求めることができます。さらに、`length` プロパティは `String` クラスのプロパティなので、他の関数やメソッドと同様に使用することができます。

```
Kotlin
val str = "Hello, world!"
println(str.length) // Output: 13
```

また、Kotlinの `String` クラスには、文字列の長さ以外にも多くの便利なメソッドが用意されています。例えば、 `substring()` というメソッドを使用すると、特定の範囲の文字列を取得することができます。

```
Kotlin
val str = "Hello, world!"
println(str.substring(0, 5)) // Output: Hello
```

## ディープダイブ

文字列の長さを求めるために、Kotlinではどのようなアルゴリズムが使用されているかを詳しく見てみましょう。

実際には、Kotlinの `String` クラスの `length` プロパティは内部的に `String.length()` メソッドを呼び出しています。このメソッドでは、Javaの `String` クラスで使用されている `count()` メソッドと同様に、文字列のUnicodeコードポイント数を返します。つまり、日本語のように1文字あたりに複数のUnicodeコードポイントが必要になる文字列を扱う場合でも、正しい文字列の長さを求めることができます。

## 関連リンク

- [KotlinのStringクラスドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [JavaのStringクラスドキュメント](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html#length())
- [Unicodeコードポイントとは？](https://ja.wikipedia.org/wiki/Unicode%E3%82%B3%E3%83%BC%E3%83%89%E3%83%9D%E3%82%A4%E3%83%B3%E3%83%88%E3%81%A8%E3%83%9E%E3%83%AB%E3%83%81%E3%83%90%E3%82%A4%E3%83%88)