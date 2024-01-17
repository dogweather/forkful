---
title:                "正規表現を使用する"
html_title:           "Kotlin: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## リギユラエクスプレッションって何？
リギユラエクスプレッションとは、文字列を検索や置換をするために使用するパターンマッチングのことです。プログラマーは、正規表現を使用することで、文字列から特定のパターンにマッチする部分を見つけたり、置換したりすることができます。

## 使い方：
```Kotlin
val pattern = Regex("\\d{2}-\\d{3}")
val result = pattern.find("私の郵便番号は12-345です")
println(result?.value)
```

出力結果：12-345

## ディープダイブ：
1. 歴史的背景：正規表現は、1960年代にリユン・トンプソンによって最初に開発されました。その後、プログラミング言語やテキストエディタなどのさまざまなツールにも導入されました。
2. 代替手段：正規表現以外にも文字列検索や置換のために使用できるツールはありますが、正規表現はより柔軟なパターンの指定や効率的な処理が可能であるため、プログラマーにとって重要なツールとなっています。
3. 実装の詳細：Kotlinでは、Regexクラスを使用して正規表現を扱います。また、infix関数によってより簡潔に正規表現を記述することができます。

## 参考:
Kotlin正規表現のドキュメント：https://kotlinlang.org/docs/tutorials/regular-expressions.html