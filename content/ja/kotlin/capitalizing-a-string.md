---
title:    "Kotlin: 文字列の先頭を大文字にする"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字化する理由は、よく見るテキストフォーマットの一つです。例えば、タイトルや見出しのスタイルを整えるために使用されます。Kotlinで文字列を大文字化する方法を学ぶことで、より効率的にコーディングできるようになります。

## 方法

文字列を大文字化するには、KotlinのStringクラスの「uppercase」メソッドを使用します。以下のコードを参考にしてください。

```Kotlin
// 文字列を定義
var str = "hello world"

// 大文字化する
str = str.uppercase()

// 出力結果
println(str) // "HELLO WORLD"
```

このように、変数に格納された文字列を大文字化するには、文字列に対して「uppercase」メソッドを呼び出して、その結果を再度変数に格納します。その後、出力することで大文字化された文字列を確認することができます。

## ディープダイブ

実際のKotlinソースコードを見ると、「uppercase」メソッドはStringクラス内で次のように定義されています。

```Kotlin
fun String.uppercase(): String {
    val locale = Locale.getDefault()
    return this.toUpperCase(locale)
}
```

ここで使用されている「Locale.getDefault()」は、端末の設定に基づいてデフォルトの言語および国を取得するメソッドです。これにより、文字列を言語に応じて大文字化することが可能になります。

また、Kotlinでは「uppercase」メソッドの他にも、文字列を大文字化するための別の方法が用意されています。例えば、「toUpperCase(Locale.ENGLISH)」を使用することで英語の文字列を大文字化することができます。

## See Also

- [Kotlin String Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Kotlin Locale Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-locale/index.html)
- [Java Doc for toUpperCase() Method](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toUpperCase())