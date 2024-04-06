---
date: 2024-01-26 04:45:44.697306-07:00
description: "\u65B9\u6CD5\uFF1A Swift\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u8907\
  \u7D20\u6570\u30B5\u30DD\u30FC\u30C8\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\
  \u81EA\u5206\u305F\u3061\u306E\u3082\u306E\u3092\u4F5C\u308B\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.105955-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Swift\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u8907\
  \u7D20\u6570\u30B5\u30DD\u30FC\u30C8\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\
  \u81EA\u5206\u305F\u3061\u306E\u3082\u306E\u3092\u4F5C\u308B\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\uFF1A."
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 方法：
Swiftには組み込みの複素数サポートはありませんが、自分たちのものを作ることができます：

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // 減算、乗算などの追加の方法
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("結果: \(result.real) + \(result.imaginary)i")
// サンプル出力: 結果: 3.0 + 7.0i
```

## 深掘り
複素数は16世紀に代数方程式で登場しました。それらは量子力学、制御理論、そして多くの他の分野において不可欠です。AppleのSwiftはPythonやC++のような言語と異なり、複素数のための標準ライブラリを持っていません。独自に実装する以外の代替手段には、複素数サポートが含まれるNumericsパッケージの使用や、Swiftの相互運用性を用いたC++の複素数ライブラリのラッピングがあります。

## 参照
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
