---
title:                "重构"
aliases:
- zh/go/refactoring.md
date:                  2024-02-03T18:07:08.531977-07:00
model:                 gpt-4-0125-preview
simple_title:         "重构"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/refactoring.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

编程中的重构涉及重组现有的计算机代码——改变代码的构造——而不改变其外部行为。程序员进行这一过程以提高代码的可读性、降低复杂性并增强可维护性，最终使软件更容易理解和修改。

## 如何进行：

在 Go 语言中，重构可以从简单的代码调整到更复杂的变更不等。让我们从一个基本示例开始：简化一个初始的 Go 函数，以获得更好的可读性和效率。

**重构前：**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    var total float64
    if quantity > 0 {
        total = float64(quantity) * price
    } else {
        total = 0
    }
    return total
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // 输出：59.9
}
```

**重构后：**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    if quantity > 0 {
        return float64(quantity) * price
    }
    return 0
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // 输出：59.9
}
```

在重构的版本中，`else` 被移除，这简化了函数的流程而不影响其输出——这是 Go 中一个基本但有影响的重构技术示例。

对于一个更高级的示例，考虑将函数重构为使用接口，以获得更好的可重用性和可测试性：

**重构前：**

```go
package main

import "fmt"

type Logger struct{}

func (l Logger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // 设想这里有一些数据处理
    logger.Log("Data processed")
}

func main() {
    logger := Logger{}
    ProcessData("example data", logger)
}
```

**重构后：**

```go
package main

import "fmt"

type Logger interface {
    Log(message string)
}

type ConsoleLogger struct{}

func (c ConsoleLogger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // 数据处理保持不变
    logger.Log("Data processed")
}

func main() {
    logger := ConsoleLogger{}
    ProcessData("example data", logger)
}
```

将函数重构为使用接口（`Logger`）而不是具体类型（`ConsoleLogger`）提高了函数的灵活性，并将数据处理与具体的日志实现解耦。

## 深入探讨

Go 语言中的重构必须在简单性（Go 的核心哲学之一）与大型软件项目所需的灵活性之间取得平衡。鉴于 Go 对功能的极简主义方法——直到最近之前都没有泛型，并且强调可读性——该语言自然引导开发者朝向更简单、更可维护的代码结构。然而，这并不意味着 Go 代码不需要重构；这意味着重构必须始终优先考虑清晰性和简单性。

从历史上看，Go 缺乏某些功能（例如，在 Go 1.18 之前的泛型）导致为了代码复用和灵活性而创造性但有时复杂的解决方案，使得为了抽象而重构成为常见做法。随着在 Go 1.18 中引入泛型，Go 开发者现在正在重构遗留代码以利用这一特性获得更好的类型安全性和代码复用性，展示了 Go 中重构实践的不断发展。

尽管如此，Go 的工具集，包括 `gofmt` 用于代码格式化和 `go vet` 用于识别可疑构造，支持维护清洁的代码库，减少了对广泛重构的需求。虽然重构是 Go 程序员工具箱中的一个宝贵工具，但从一开始就明智地使用 Go 的语言特性和工具可以帮助最小化后期复杂重构的需要。
