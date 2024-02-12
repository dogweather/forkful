---
title:                "将代码组织成函数"
aliases: - /zh/swift/organizing-code-into-functions.md
date:                  2024-01-26T01:16:11.540529-07:00
model:                 gpt-4-0125-preview
simple_title:         "将代码组织成函数"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
将代码组织成函数，就是将任务细分成可重用的块。这使得代码更加清晰，减少错误，并且更易于调试或重构。

## 如何实现：
想象一个任务：计算数组的平均值。如果不使用函数，你可能会将所有代码都放在 main 中。使用函数，则可以这样做：

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// 使用方法
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("平均分是 \(averageScore)")
```

示例输出将是：
```
平均分是 87.6875
```

## 深入探讨
历史上，随着编程变得复杂，函数成为管理复杂性的基石。其他选择包括内联编码和复制粘贴代码（意大利面代码）——现在这些通常被认为是坏习惯。在 Swift 中，函数是一等公民；它们可以被赋值给变量，作为参数传递，以及从其他函数返回，使得代码更加模块化和灵活。

在实现上，设计你的函数要做好一件事。力求函数具有清晰的目的，且名称反映了这一点。注意参数的数量——如果太多，你可能做得太多了。错误处理怎么办？考虑使用抛出函数并优雅地处理问题。记住：Swift 讲究可读性和易于维护。

## 参考资料
- [Swift 编程语言指南 - 函数](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Ray Wenderlich 的 Swift 风格指南](https://github.com/raywenderlich/swift-style-guide)
- [Martin Fowler 的《重构：改善既有代码的设计》](https://martinfowler.com/books/refactoring.html)
