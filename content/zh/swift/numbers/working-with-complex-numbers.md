---
title:                "处理复数"
aliases:
- /zh/swift/working-with-complex-numbers.md
date:                  2024-01-26T04:45:55.737441-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理复数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
复数具有实部和虚部（就像 3 + 4i）。Swift 程序员会在信号处理、解决某些数学问题以及模拟物理等任务中使用它们。

## 如何操作:
Swift 没有内置的复数支持，但我们可以自己动手做：

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // 其他方法如减法、乘法等
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("结果：\(result.real) + \(result.imaginary)i")
// 示例输出：结果：3.0 + 7.0i
```

## 深入了解
复数在 16 世纪代数方程中出现。它们在量子力学、控制理论和许多其他领域都至关重要。与 Python 或 C++ 等语言不同，Apple 的 Swift 没有用于复数的标准库。除了自己动手外的替代方案包括使用包含复数支持的数学包或者利用 Swift 的互操作性封装 C++ 复数库。

## 另请参阅
- Swift 数值学：[https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
