---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:23.199668-07:00
description: "\u5982\u4F55\uFF1A \u5728 Go \u4E2D\uFF0C\u6D4B\u8BD5\u901A\u5E38\u5199\
  \u5728\u4E0E\u5B83\u4EEC\u6D4B\u8BD5\u7684\u4EE3\u7801\u76F8\u540C\u7684\u5305\u4E2D\
  \u3002\u5305\u542B\u6D4B\u8BD5\u7684\u6587\u4EF6\u540D\u4EE5 `_test.go` \u4E3A\u540E\
  \u7F00\u3002\u6D4B\u8BD5\u662F\u91C7\u7528\u6307\u5411 testing.T \u5BF9\u8C61\uFF08\
  \u6765\u81EA `testing` \u5305\uFF09\u7684\u6307\u9488\u4F5C\u4E3A\u53C2\u6570\u7684\
  \u51FD\u6570\uFF0C\u5E76\u901A\u8FC7\u8C03\u7528\u5982 `t.Fail()`\u3001`t.Errorf()`\
  \ \u7B49\u65B9\u6CD5\u6765\u6807\u793A\u5931\u8D25\u3002 \u4E00\u4E2A\u7B80\u5355\
  \u7684\u9488\u5BF9\u5728\u2026"
lastmod: '2024-04-05T21:53:47.506629-06:00'
model: gpt-4-0125-preview
summary: "\u4E00\u4E2A\u7B80\u5355\u7684\u9488\u5BF9\u5728 `math.go` \u4E2D\u5B9A\u4E49\
  \u7684\u51FD\u6570 `Add` \u7684\u6D4B\u8BD5\u793A\u4F8B\uFF1A."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 如何：
在 Go 中，测试通常写在与它们测试的代码相同的包中。包含测试的文件名以 `_test.go` 为后缀。测试是采用指向 testing.T 对象（来自 `testing` 包）的指针作为参数的函数，并通过调用如 `t.Fail()`、`t.Errorf()` 等方法来标示失败。

一个简单的针对在 `math.go` 中定义的函数 `Add` 的测试示例：
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

测试文件 `math_test.go`：
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; want %d", result, expected)
    }
}
```

在与测试文件相同的目录下使用 `go test` 命令运行测试。表示测试通过的示例输出可能类似于：

```
PASS
ok      example.com/my/math 0.002s
```

对于表格驱动的测试，这使您能够有效测试各种输入和输出组合，定义代表测试案例的结构体切片：

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("got %d, want %d", ans, tt.expected)
            }
        })
    }
}
```

## 深入探索
Go 测试框架在 Go 1 与语言本身一起引入，旨在与 Go 工具链无缝集成，反映了 Go 在软件开发中对简约和效率的强调。与依赖外部库或复杂设置的其他语言中的一些测试框架不同，Go 的内置 `testing` 包为编写和运行测试提供了一种直接的方式。

Go 测试方法的一个有趣方面是它采用的约定优于配置原则，比如文件命名模式 (`_test.go`) 和对标准库功能而非外部依赖的使用。这种简约性方法鼓励开发者编写测试，因为入门的门槛很低。

虽然 Go 的内置测试功能覆盖了很多范围，但在某些情况下，第三方工具或框架可能提供更多功能，如模拟生成、模糊测试或行为驱动开发（BDD）样式的测试。Testify 或 GoMock 等流行库补充了 Go 的标准测试能力，提供了更表达性的断言或模拟生成能力，这在具有许多依赖关系的复杂应用程序中特别有用。

尽管有这些替代品的存在，标准 Go 测试包仍然是 Go 测试的基石，因为它简单、性能良好并与语言和工具链紧密集成。无论开发人员是否选择使用第三方工具增强它，Go 测试框架都为确保代码质量和可靠性提供了坚实的基础。
