---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何とどうして？
検索と置換は、特定のテキストを見つけて（検索）それを新しいテキストで置き換える（置換）ことです。プログラマーはこれを用いて、コード内の特定の文字列を一括で変更したり、データの一部を新しい情報で更新するときに使用します。

## どうやって：
```Kotlin
fun main() {
  var text = "Kotlin programming is fun"
  println(text)
  // Output: Kotlin programming is fun

  text = text.replace("fun", "awesome")
  println(text)
  // Output: Kotlin programming is awesome
}
```
この例では、私たちは "fun"という文字列を "awesome"に置替えています。

## ディープダイブ
検索と置換の概念は、ワードプロセッサーが一般的になった1970年代以来、ITの中核的な部分となってきました。この機能は非常に強力で、一連の長いテキストやコードベース内の単語やフレーズをすばやく、正確に見つけて置き換えることができます。代替手段としては手動での検索と置換がありますが、大量のテキストやコード中での使用には向いていません。Kotlinは `replace()` 関数を用いてこの機能を実装していますが、他の言語では異なるメソッドや関数を使用することがあります。

## 関連資料
- Kotlin公式ドキュメント: [String operations](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- StackOverflowのディスカッション: [Search and replace in Kotlin](https://stackoverflow.com/questions/36574183/how-to-replace-substring-in-kotlin)