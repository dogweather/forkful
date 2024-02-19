---
aliases:
- /zh/go/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:23.199668-07:00
description: "\u5728 Go \u4E2D\u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\u5C0F\
  \u578B\u3001\u53EF\u7BA1\u7406\u7684\u4EE3\u7801\u7247\u6BB5\uFF0C\u7528\u4E8E\u9A8C\
  \u8BC1\u5E94\u7528\u7A0B\u5E8F\u7684\u529F\u80FD\u548C\u884C\u4E3A\u3002\u7A0B\u5E8F\
  \u5458\u7F16\u5199\u6D4B\u8BD5\u4EE5\u786E\u4FDD\u5728\u5404\u79CD\u6761\u4EF6\u4E0B\
  \u4ED6\u4EEC\u7684\u4EE3\u7801\u5982\u9884\u671F\u90A3\u6837\u5DE5\u4F5C\uFF0C\u4EE5\
  \u4FBF\u4E8E\u91CD\u6784\uFF0C\u5E76\u5E2E\u52A9\u9632\u6B62\u56DE\u5F52\u3002"
lastmod: 2024-02-18 23:08:58.711644
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\u5C0F\u578B\
  \u3001\u53EF\u7BA1\u7406\u7684\u4EE3\u7801\u7247\u6BB5\uFF0C\u7528\u4E8E\u9A8C\u8BC1\
  \u5E94\u7528\u7A0B\u5E8F\u7684\u529F\u80FD\u548C\u884C\u4E3A\u3002\u7A0B\u5E8F\u5458\
  \u7F16\u5199\u6D4B\u8BD5\u4EE5\u786E\u4FDD\u5728\u5404\u79CD\u6761\u4EF6\u4E0B\u4ED6\
  \u4EEC\u7684\u4EE3\u7801\u5982\u9884\u671F\u90A3\u6837\u5DE5\u4F5C\uFF0C\u4EE5\u4FBF\
  \u4E8E\u91CD\u6784\uFF0C\u5E76\u5E2E\u52A9\u9632\u6B62\u56DE\u5F52\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Go 中编写测试涉及创建小型、可管理的代码片段，用于验证应用程序的功能和行为。程序员编写测试以确保在各种条件下他们的代码如预期那样工作，以便于重构，并帮助防止回归。

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
