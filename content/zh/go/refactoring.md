---
title:                "重构代码"
date:                  2024-01-26T01:37:02.387205-07:00
model:                 gpt-4-0125-preview
simple_title:         "重构代码"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/refactoring.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
重构是不改变现有计算机代码外部行为的情况下，对其进行结构重组的过程。程序员进行重构是为了提高软件的非功能属性，如可读性和可维护性，这可以使代码更易于理解，降低复杂性，并帮助更容易发现错误。

## 如何操作:
让我们深入一个简单的Go代码重构示例。我们将取一个计算数字切片平均值的代码片段，并对其进行重构以提高清晰度和可重用性。

原始代码:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Average:", average)
}
```

重构代码:
```Go
package main

import "fmt"

// CalculateAverage 接收一个float64的切片并返回平均值。
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Average:", average)
}
```

在重构的代码中，我们已经将计算平均值的逻辑提取到一个单独名为`CalculateAverage`的函数中。这使得`main`函数更加简洁，且平均值计算逻辑可重用且可测试。

## 深入探讨
重构代码并不是一个现代概念；它早在计算机广泛使用之前就已存在。这种做法很可能始于机械工程领域，或者更早。在软件中，随着面向对象编程和极限编程（XP）在1990年代的出现而变得更加正式，特别受到Martin Fowler的开创性著作《重构：改善既有代码的设计》的影响。

重构技术有很多，从简单的变量重命名以提高清晰度到更复杂的模式，如提取方法或类。关键是进行小的、渐进式的更改，这些更改不会修改软件的功能，但会改善内部结构。

在使用Go进行重构时，由于该语言的简单性和强大的标准库，重构可能会非常直接。然而，拥有一套好的单元测试仍然很重要，以确保重构不会引入错误。工具如`gorename`和`gofmt`帮助自动化部分过程，IDE通常也具有内置的重构支持。

除了手动重构，还有一些自动化Go代码重构工具可用，例如GoLand的重构工具和Go Refactor。虽然这些可以加速过程，但它们不是理解代码和进行考虑周到的更改的替代品。

## 参见
 - [在Go中重构：简洁即美](https://go.dev/blog/slices)
 - [有效的Go：使用接口重构](https://go.dev/doc/effective_go#interfaces)
 - [Martin Fowler的重构页面](https://refactoring.com/)
 - [GoLand重构工具](https://www.jetbrains.com/go/features/refactorings/)