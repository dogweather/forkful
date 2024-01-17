---
title:                "テキストの検索と置換"
html_title:           "Kotlin: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なに & なぜ？
検索と置換は、プログラマーがテキスト内で特定の語句を見つけ、それを新しい語句に置き換えることを指します。プログラマーは、コードやドキュメント内の特定の語句を一括で変更するために、この機能を使用します。

## 使い方：
#### 文字列の置換：
```Kotlin
val oldString = "Hello World"
val newString = oldString.replace("World", "Universe")
println(newString)

// Output: Hello Universe
```

#### 正規表現の置換：
```Kotlin
val sentence = "I have 10 cats and 5 dogs."
val newSentence = sentence.replace(Regex("[0-9]+"), "3")
println(newSentence)

// Output: I have 3 cats and 3 dogs.
```

## 詳しく見ていきましょう：
検索と置換は、テキスト処理において非常に重要な機能です。かつては、手作業で文書中の誤字や印刷ミスを修正する必要がありましたが、検索と置換を使うことで簡単に修正することができるようになりました。もしKotlinを使わない場合、Javaでは正規表現を使って実現することができます。

## 参考リンク：
- Kotlin Strings and Regular Expressions: https://kotlinlang.org/docs/reference/regular-expressions.html
- Java Pattern Class: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html
- Online Regex Tester: https://regexr.com/