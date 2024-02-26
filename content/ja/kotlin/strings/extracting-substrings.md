---
date: 2024-01-20 17:46:13.238616-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u90E8\u5206\u6587\u5B57\u5217\u3092\u53D6\
  \u5F97\u3059\u308B\u3053\u3068\u306F\u3001\u6587\u5B57\u5217\u5185\u306E\u7279\u5B9A\
  \u306E\u30BB\u30AF\u30B7\u30E7\u30F3\u3092\u5207\u308A\u51FA\u3059\u884C\u70BA\u3067\
  \u3059\u3002\u30C7\u30FC\u30BF\u306E\u4E00\u90E8\u3092\u8868\u793A\u3057\u305F\u308A\
  \u89E3\u6790\u3057\u305F\u308A\u3059\u308B\u5834\u5408\u306B\u884C\u308F\u308C\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.075295-07:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u90E8\u5206\u6587\u5B57\u5217\u3092\u53D6\
  \u5F97\u3059\u308B\u3053\u3068\u306F\u3001\u6587\u5B57\u5217\u5185\u306E\u7279\u5B9A\
  \u306E\u30BB\u30AF\u30B7\u30E7\u30F3\u3092\u5207\u308A\u51FA\u3059\u884C\u70BA\u3067\
  \u3059\u3002\u30C7\u30FC\u30BF\u306E\u4E00\u90E8\u3092\u8868\u793A\u3057\u305F\u308A\
  \u89E3\u6790\u3057\u305F\u308A\u3059\u308B\u5834\u5408\u306B\u884C\u308F\u308C\u307E\
  \u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
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
