---
title:                "文字列の長さを見つける"
html_title:           "Kotlin: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

Kotlinのプログラムを使用して、文字列の長さを確認する理由は、テキスト処理において基本的な操作であり、文字列を操作する際には必須の知識であるからです。

## 方法

文字列の長さを確認するには、Kotlinの「.length」メソッドを使用します。「.length」メソッドは、文字列の長さを整数値で返します。以下のコードを参考にしてください。

```Kotlin
// 文字列の長さを確認する例
val str = "こんにちは、世界！"
println(str.length) // 出力：11
```

## 深堀り

Kotlinでは、文字列を表すデータ型として「String」クラスを使用します。そして、「String」クラスには「.length」メソッドの他にも多くの便利なメソッドが用意されています。例えば、文字列を反転させる「.reversed()」メソッドや、特定の文字列が含まれているかを確認する「.contains()」メソッドなどがあります。

## 参考リンク

- [Kotlin: 文字列操作](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin: Stringクラス](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Kotlin: Stringメソッド一覧](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/length.html)

## 関連リンク

- [Kotlin for Android: 公式ドキュメント](https://developer.android.com/kotlin/)
- [Kotlinの基礎: 文字列操作](https://qiita.com/opengl-8080/items/34402cf9fbf665138171)
- [Kotlin: Stringメソッドの使い方まとめ](https://qiita.com/lando/items/3a9cfc67d3a88ae095d1)