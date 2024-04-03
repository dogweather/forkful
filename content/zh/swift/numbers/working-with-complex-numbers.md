---
date: 2024-01-26 04:45:55.737441-07:00
description: "\u590D\u6570\u5177\u6709\u5B9E\u90E8\u548C\u865A\u90E8\uFF08\u5C31\u50CF\
  \ 3 + 4i\uFF09\u3002Swift \u7A0B\u5E8F\u5458\u4F1A\u5728\u4FE1\u53F7\u5904\u7406\
  \u3001\u89E3\u51B3\u67D0\u4E9B\u6570\u5B66\u95EE\u9898\u4EE5\u53CA\u6A21\u62DF\u7269\
  \u7406\u7B49\u4EFB\u52A1\u4E2D\u4F7F\u7528\u5B83\u4EEC\u3002"
lastmod: '2024-03-13T22:44:48.150764-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u5177\u6709\u5B9E\u90E8\u548C\u865A\u90E8\uFF08\u5C31\u50CF\
  \ 3 + 4i\uFF09\u3002Swift \u7A0B\u5E8F\u5458\u4F1A\u5728\u4FE1\u53F7\u5904\u7406\
  \u3001\u89E3\u51B3\u67D0\u4E9B\u6570\u5B66\u95EE\u9898\u4EE5\u53CA\u6A21\u62DF\u7269\
  \u7406\u7B49\u4EFB\u52A1\u4E2D\u4F7F\u7528\u5B83\u4EEC\u3002."
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

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
