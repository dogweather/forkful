---
date: 2024-01-26 04:45:55.737441-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u590D\u6570\u5728 16 \u4E16\u7EAA\u4EE3\u6570\
  \u65B9\u7A0B\u4E2D\u51FA\u73B0\u3002\u5B83\u4EEC\u5728\u91CF\u5B50\u529B\u5B66\u3001\
  \u63A7\u5236\u7406\u8BBA\u548C\u8BB8\u591A\u5176\u4ED6\u9886\u57DF\u90FD\u81F3\u5173\
  \u91CD\u8981\u3002\u4E0E Python \u6216 C++ \u7B49\u8BED\u8A00\u4E0D\u540C\uFF0C\
  Apple \u7684 Swift \u6CA1\u6709\u7528\u4E8E\u590D\u6570\u7684\u6807\u51C6\u5E93\u3002\
  \u9664\u4E86\u81EA\u5DF1\u52A8\u624B\u5916\u7684\u66FF\u4EE3\u65B9\u6848\u5305\u62EC\
  \u4F7F\u7528\u5305\u542B\u590D\u6570\u652F\u6301\u7684\u6570\u5B66\u5305\u6216\u8005\
  \u5229\u7528 Swift \u7684\u4E92\u64CD\u4F5C\u6027\u5C01\u88C5 C++ \u590D\u6570\u5E93\
  \u3002"
lastmod: '2024-04-05T22:51:01.358604-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u5728 16 \u4E16\u7EAA\u4EE3\u6570\u65B9\u7A0B\u4E2D\u51FA\u73B0\
  \u3002\u5B83\u4EEC\u5728\u91CF\u5B50\u529B\u5B66\u3001\u63A7\u5236\u7406\u8BBA\u548C\
  \u8BB8\u591A\u5176\u4ED6\u9886\u57DF\u90FD\u81F3\u5173\u91CD\u8981\u3002\u4E0E Python\
  \ \u6216 C++ \u7B49\u8BED\u8A00\u4E0D\u540C\uFF0CApple \u7684 Swift \u6CA1\u6709\
  \u7528\u4E8E\u590D\u6570\u7684\u6807\u51C6\u5E93\u3002\u9664\u4E86\u81EA\u5DF1\u52A8\
  \u624B\u5916\u7684\u66FF\u4EE3\u65B9\u6848\u5305\u62EC\u4F7F\u7528\u5305\u542B\u590D\
  \u6570\u652F\u6301\u7684\u6570\u5B66\u5305\u6216\u8005\u5229\u7528 Swift \u7684\u4E92\
  \u64CD\u4F5C\u6027\u5C01\u88C5 C++ \u590D\u6570\u5E93\u3002"
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
