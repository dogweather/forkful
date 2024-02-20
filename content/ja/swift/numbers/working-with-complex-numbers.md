---
date: 2024-01-26 04:45:44.697306-07:00
description: "\u8907\u7D20\u6570\u306F\u5B9F\u90E8\u3068\u865A\u90E8\uFF08\u4F8B\uFF1A\
  3 + 4i\uFF09\u3092\u6301\u3063\u3066\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u4FE1\u53F7\u51E6\u7406\u3001\u7279\u5B9A\u306E\u6570\u5B66\
  \u554F\u984C\u306E\u89E3\u6C7A\u3001\u7269\u7406\u306E\u30B7\u30DF\u30E5\u30EC\u30FC\
  \u30B7\u30E7\u30F3\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306BSwift\u3067\u305D\u308C\
  \u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.716740
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u5B9F\u90E8\u3068\u865A\u90E8\uFF08\u4F8B\uFF1A\
  3 + 4i\uFF09\u3092\u6301\u3063\u3066\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u4FE1\u53F7\u51E6\u7406\u3001\u7279\u5B9A\u306E\u6570\u5B66\
  \u554F\u984C\u306E\u89E3\u6C7A\u3001\u7269\u7406\u306E\u30B7\u30DF\u30E5\u30EC\u30FC\
  \u30B7\u30E7\u30F3\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306BSwift\u3067\u305D\u308C\
  \u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は実部と虚部（例：3 + 4i）を持っています。プログラマーは、信号処理、特定の数学問題の解決、物理のシミュレーションなどのタスクにSwiftでそれらを使用します。

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
