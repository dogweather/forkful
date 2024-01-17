---
title:                "文字列の連結"
html_title:           "Kotlin: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何をして、なぜ?

文字列を連結するとは、複数の文字列を一つにまとめることです。プログラマーは、文字列を連結することで、より複雑な文字列を作成できます。

## 方法:

```Kotlin
// 文字列を連結する方法
val name = "田中"
val age = 25
val message = "私の名前は" + name + "です。年齢は" + age + "歳です。"
println(message)

// Output:
// 私の名前は田中です。年齢は25歳です。
```

## 詳細:

文字列の連結は、一つの文字列を作成するための一番簡単な方法です。実際、多くのプログラミング言語で、文字列を連結するための専用の演算子が用意されています。Kotlinでは、プラス記号（+）を使用して文字列を連結することができます。

Kotlinでは、文字列の連結に加えて、テンプレート文字列を使用することもできます。テンプレート文字列では、変数や式を文字列の中に埋め込むことができます。例えば、先ほどのコードをテンプレート文字列を使って書くと次のようになります。

```Kotlin
// テンプレート文字列を使った文字列の連結
val name = "田中"
val age = 25
val message = "私の名前は$nameです。年齢は$age歳です。"
println(message)

// Output:
// 私の名前は田中です。年齢は25歳です。
```

## 参考:

- [Kotlin Documentation](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin String Concatenation with Examples](https://www.opentechguides.com/how-to/article/kotlin/106/kotlin-string-concatenation.html)
- [Kotlin String Templates](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)