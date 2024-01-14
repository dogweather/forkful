---
title:                "Kotlin: テキストを検索および置換する"
simple_title:         "テキストを検索および置換する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換をすることの理由は、テキストの修正や更新を効率的に行うためです。特に、大量のテキストファイルを処理する場合には、手作業ではなくプログラミングを使って自動化することで、時間と手間を節約することができます。

## 方法

Kotlinでは、Stringクラスの`replace()`メソッドを使ってテキストの検索と置換を行うことができます。以下のコードを参考にしてください。

```Kotlin
val text = "Apple, Banana, Orange, Apple"
val replaceText = text.replace("Apple", "Mango")

println(replaceText)// 出力: Mango, Banana, Orange, Mango
```

上記の例では、`replace()`メソッドの第一引数に検索したい文字列、第二引数に置換したい文字列を指定しています。この場合、最初に見つかった「Apple」が「Mango」に置換されます。また、置換された新しい文字列が返されるので、変数に代入して使用することもできます。

必要に応じて、`replace()`メソッドに第三引数として置換する最大回数を指定することもできます。また、正規表現を使った検索や置換も可能です。

## 詳細を深く掘り下げる

Kotlinでは、Stringクラスの他にもRegexクラスを使うことで、より詳細な検索と置換を行うことができます。正規表現を使用することで、より柔軟なパターンマッチングが可能になります。詳細については公式ドキュメントをご参照ください。

## この記事の参考リソース

[The Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/replace.html)

[Regex class in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)

[正規表現入門 - Regex Tutorial](https://www.tutorialspoint.com/kotlin/regex.htm)

## 参考リソース

[テキストファイルの検索と置換を自動化する方法 - Qiita](https://qiita.com/cndr/items/3c039d693f64654be40b)

[Kotlinで文字列を置換する方法 - Love Java](https://lovejava.net/blog/2020/07/27/kotlin%E3%81%A7%E6%96%87%E5%AD%97%E5%88%97%E3%82%92%E7%BD%AE%E6%8F%9B%E3%81%99%E3%82%8B%E6%96%B9%E6%B3%95/)

[テキストの検索と置換を自動化する - Javaholic](https://javaholic.hatenablog.com/entry/2020/09/02/124734)