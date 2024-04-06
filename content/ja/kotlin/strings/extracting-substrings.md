---
date: 2024-01-20 17:46:13.238616-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.935828-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6587\u5B57\u5217\u64CD\u4F5C\u306F\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30DF\u30F3\u30B0\u306E\u9ECE\u660E\u671F\u304B\u3089\u30B3\u30F3\u30D4\u30E5\
  \u30FC\u30BF\u30B5\u30A4\u30A8\u30F3\u30B9\u306E\u57FA\u672C\u7684\u306A\u8981\u7D20\
  \u3067\u3059\u3002Kotlin\u3067\u306F\u3001`substring`\u95A2\u6570\u306F\u30B7\u30F3\
  \u30D7\u30EB\u3067\u76F4\u611F\u7684\u306AAPI\u3092\u63D0\u4F9B\u3057\u3001String\u30AF\
  \u30E9\u30B9\u306E\u4E00\u90E8\u3068\u3057\u3066\u5B9F\u88C5\u3055\u308C\u3066\u3044\
  \u307E\u3059\u3002Java\u306EString\u30AF\u30E9\u30B9\u304B\u3089\u7D99\u627F\u3055\
  \u308C\u305F\u3053\u306E\u30E1\u30BD\u30C3\u30C9\u306F\u3001\u5805\u7262\u306A\u6587\
  \u5B57\u5217\u51E6\u7406\u80FD\u529B\u3092\u53D7\u3051\u7D99\u304E\u306A\u304C\u3089\
  \u3001Kotlin\u3089\u3057\u3044\u6D17\u7DF4\u3055\u308C\u305F\u547C\u3073\u51FA\u3057\
  \u65B9\u3092\u3057\u307E\u3059\u3002`substring`\u306B\u306F\u958B\u59CB\u30A4\u30F3\
  \u30C7\u30C3\u30AF\u30B9\u3060\u3051\u3067\u306F\u306A\u304F\u3001\u7D42\u4E86\u30A4\
  \u30F3\u30C7\u30C3\u30AF\u30B9\u3092\u6307\u5B9A\u3059\u308B\u30AA\u30FC\u30D0\u30FC\
  \u30ED\u30FC\u30C9\u3055\u308C\u305F\u30D0\u30FC\u30B8\u30E7\u30F3\u3082\u5B58\u5728\
  \u3057\u3001\u67D4\u8EDF\u306A\u64CD\u4F5C\u304C\u53EF\u80FD\u3067\u3059\u3002\u4EE3\
  \u66FF\u65B9\u6CD5\u3068\u3057\u3066\u3001`take`\u3084`drop`\u306E\u3088\u3046\u306A\
  \u62E1\u5F35\u95A2\u6570\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u3082\u3067\u304D\
  \u307E\u3059\u304C\u3001\u660E\u78BA\u306A\u7BC4\u56F2\u3092\u6307\u5B9A\u3057\u3066\
  \u5207\u308A\u51FA\u3059\u3068\u304D\u306F`substring`\u304C\u3082\u3063\u3068\u3082\
  \u4E00\u822C\u7684\u3067\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

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
