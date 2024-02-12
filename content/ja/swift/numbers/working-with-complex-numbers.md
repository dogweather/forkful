---
title:                "複素数の扱い方"
aliases:
- /ja/swift/working-with-complex-numbers.md
date:                  2024-01-26T04:45:44.697306-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-complex-numbers.md"
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
