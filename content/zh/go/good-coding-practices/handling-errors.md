---
title:                "处理错误"
aliases:
- /zh/go/handling-errors.md
date:                  2024-02-03T17:58:05.857459-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理错误"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

Go 语言中处理错误涉及识别和响应程序中的错误情况。程序员进行错误处理，以确保他们的应用程序能够从意外情况中优雅地恢复，从而使软件更加稳健可靠。

## 如何操作：

在 Go 中，错误处理通过显式管理 `error` 类型来实现。可能失败的函数会将错误作为它们的最后一个返回值返回。检查这个错误值是否为 `nil` 将告诉你是否发生了错误。

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("value must be 100 or less")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Error:", err)
    } else {
        fmt.Println("Result:", result)
    }
    
    // 优雅地处理错误
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Error:", anotherErr)
    } else {
        fmt.Println("Result:", anotherResult)
    }
}
```

上述代码的示例输出：
```
Error: value must be 100 or less
Result: 100
```

在这个例子中，`Compute` 函数或者返回一个计算值或者返回一个错误。调用者通过检查 `err` 是否不为 `nil` 来处理错误。

## 深入了解

Go 的错误处理方法是故意保持简单直接且类型安全的，需要显式检查错误。这种概念与在 Java 和 Python 等语言中看到的基于异常的错误处理形成对比，其中错误会沿着调用栈向上传播，除非被异常处理器捕获。Go 团队认为，显式地处理错误会导致更清晰、更可靠的代码，因为它迫使程序员立即处理发生的错误所在处。

然而，一些批评指出，这种模式可能导致冗长的代码，特别是在具有许多容易出错操作的复杂函数中。作为回应，Go 的新版本引入了更复杂的错误处理功能，例如错误包装，使在不丢失原始错误信息的情况下为错误提供上下文变得更容易。社区还看到了关于新的错误处理机制（如 check/handle）的提议，尽管这些到我最后更新时仍在讨论中。

Go 的错误处理哲学强调理解和计划错误作为程序正常流程的一部分。这种方法促进了更加弹性和可预测的软件开发，尽管可能会增加样板代码量。对于特别复杂的情况，存在替代模式和库来简化错误处理，但 Go 内置的 `error` 类型仍然是该语言中错误处理的基础。
