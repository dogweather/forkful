---
title:                "部分文字列の抽出"
date:                  2024-01-20T17:46:13.238616-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から部分文字列を取得することは、文字列内の特定のセクションを切り出す行為です。データの一部を表示したり解析したりする場合に行われます。

## How to: (方法)
```Kotlin
fun main() {
    val str = "こんにちは、世界！"
    val sub1 = str.substring(0, 5) // 最初から5文字目まで
    val sub2 = str.substring(5)    // 6文字目から最後まで

    println(sub1) // 出力: こんにちは
    println(sub2) // 出力: 、世界！
}
```

## Deep Dive (詳細情報)
文字列操作は、プログラミングの黎明期からコンピュータサイエンスの基本的な要素です。Kotlinでは、`substring`関数はシンプルで直感的なAPIを提供し、Stringクラスの一部として実装されています。JavaのStringクラスから継承されたこのメソッドは、堅牢な文字列処理能力を受け継ぎながら、Kotlinらしい洗練された呼び出し方をします。`substring`には開始インデックスだけではなく、終了インデックスを指定するオーバーロードされたバージョンも存在し、柔軟な操作が可能です。代替方法として、`take`や`drop`のような拡張関数を利用することもできますが、明確な範囲を指定して切り出すときは`substring`がもっとも一般的です。

## See Also (関連情報)
- Kotlinの公式ドキュメンテーションでの`String`クラス: [Kotlin Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)