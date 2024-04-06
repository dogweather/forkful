---
date: 2024-01-26 01:16:26.101645-07:00
description: "\u65B9\u6CD5 \u3042\u308B\u30BF\u30B9\u30AF\u3092\u60F3\u50CF\u3057\u3066\
  \u304F\u3060\u3055\u3044\uFF1A\u914D\u5217\u306E\u5E73\u5747\u3092\u8A08\u7B97\u3059\
  \u308B\u3002\u95A2\u6570\u304C\u306A\u3051\u308C\u3070\u3001\u305D\u308C\u3092\u3059\
  \u3079\u3066\u30E1\u30A4\u30F3\u306B\u5165\u308C\u307E\u3059\u3002\u95A2\u6570\u3092\
  \u4F7F\u7528\u3059\u308B\u5834\u5408\u3001\u6B21\u306E\u3088\u3046\u306B\u3057\u307E\
  \u3059\u3002"
lastmod: '2024-04-05T22:38:42.119173-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5 \u3042\u308B\u30BF\u30B9\u30AF\u3092\u60F3\u50CF\u3057\u3066\
  \u304F\u3060\u3055\u3044\uFF1A\u914D\u5217\u306E\u5E73\u5747\u3092\u8A08\u7B97\u3059\
  \u308B\u3002\u95A2\u6570\u304C\u306A\u3051\u308C\u3070\u3001\u305D\u308C\u3092\u3059\
  \u3079\u3066\u30E1\u30A4\u30F3\u306B\u5165\u308C\u307E\u3059\u3002\u95A2\u6570\u3092\
  \u4F7F\u7528\u3059\u308B\u5834\u5408\u3001\u6B21\u306E\u3088\u3046\u306B\u3057\u307E\
  \u3059\u3002"
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

## 方法
あるタスクを想像してください：配列の平均を計算する。関数がなければ、それをすべてメインに入れます。関数を使用する場合、次のようにします。

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// 使用法
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("Average score is \(averageScore)")
```

サンプル出力は次のようになります：
```
Average score is 87.6875
```

## 深く掘り下げる
歴史的に、プログラミングが複雑になるにつれて、関数は複雑さを管理するための重要な要素になりました。代替手段には、インラインコーディングやコードのコピー＆ペースト（スパゲッティコード）が含まれますが、これらは今では一般的に悪い習慣と見なされています。Swiftでは、関数はファーストクラスの市民です。変数に割り当てたり、引数として渡したり、他の関数から戻り値として返すことができるため、コードをよりモジュール式で柔軟にします。

実装においては、あなたの関数が一つのことをうまく行うように設計します。明確な目的とそれを反映する名前を持つ関数を目指してください。パラメータの数に注意してください。多すぎると、あなたが多くのことをしている可能性があります。エラーハンドリングについては？問題を上手に処理するために、投げる関数を検討してください。覚えておいてください：Swiftは読みやすさとメンテナンスのしやすさについてです。

## 参照
- [Swiftプログラミング言語ガイド - 関数](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Ray WenderlichのSwiftスタイルガイド](https://github.com/raywenderlich/swift-style-guide)
- [Martin FowlerのRefactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
