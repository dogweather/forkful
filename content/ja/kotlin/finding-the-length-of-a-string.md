---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列の長さを調べるとは、文字列が何文字から成るかを見つけることです。コンピュータプログラミングではこれが非常に重要で、例えばユーザからの入力をチェックする際や、テキストを検索/分割する際に使用します。

## 使い方

以下にKotlinで文字列の長さを見つける方法を示します。 `length`プロパティを使用して文字列の長さを取得できます。

```Kotlin
fun main() {
    val string = "こんにちは世界"
    val length = string.length
    println("String length is $length.")
}
```

この例では、出力は `String length is 6.`となります。

## ディープダイブ

歴史的な背景については、文字列の長さを求めることはコンピュータが誕生した当初からある基本的な操作です。最初のコンピュータ言語から、現在の高度なプログラミング言語まで、すべてが何らかの形でこの機能を提供しています。

代替手段として、独自の関数を作成して各文字を一つずつ数え上げることも可能ですが、それは非効率的で、既存の`length` プロパティを使用することが推奨されます。

実装の詳細として、Kotlinでは文字列の長さは内部的に文字列が格納されている配列のインデックスとして管理されています。したがって、 `length` プロパティの呼び出しは、実質的にそのインデックスを返すだけなので対応する操作は高速です。

## 関連情報

- Kotlin公式ドキュメンテーション： [Strings](https://kotlinlang.org/docs/strings.html)
- Kotlinの文字列操作に関するチュートリアル： [Working with strings in Kotlin](https://www.programiz.com/kotlin-programming/string)
- 文字列長を効率的に操作するためのアドバイス： [Efficiency of manipulating chars in a string in Java](https://stackoverflow.com/questions/1158777/efficiency-of-manipulating-chars-in-a-string-in-java)