---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:31.040700-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Go \u4E2D\uFF0C\u5B57\u7B26\u4E32\
  \u63D2\u503C\u901A\u5E38\u901A\u8FC7\u4F7F\u7528 `fmt` \u5305\u6765\u5B9E\u73B0\uFF0C\
  \u7279\u522B\u662F `Sprintf` \u51FD\u6570\uFF0C\u5B83\u5141\u8BB8\u4F60\u901A\u8FC7\
  \u6307\u5B9A\u683C\u5F0F\u5316\u52A8\u8BCD\u5C06\u53D8\u91CF\u6CE8\u5165\u5230\u5B57\
  \u7B26\u4E32\u4E2D\u3002\u683C\u5F0F\u5316\u52A8\u8BCD\u662F\u683C\u5F0F\u5B57\u7B26\
  \u4E32\u4E2D\u7684\u5360\u4F4D\u7B26\uFF0C\u5E76\u88AB\u7ED9\u5B9A\u53D8\u91CF\u7684\
  \u503C\u6240\u66FF\u6362\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\uFF1A\
  ."
lastmod: '2024-03-13T22:44:47.124783-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\uFF0C\u5B57\u7B26\u4E32\u63D2\u503C\u901A\u5E38\u901A\u8FC7\
  \u4F7F\u7528 `fmt` \u5305\u6765\u5B9E\u73B0\uFF0C\u7279\u522B\u662F `Sprintf` \u51FD\
  \u6570\uFF0C\u5B83\u5141\u8BB8\u4F60\u901A\u8FC7\u6307\u5B9A\u683C\u5F0F\u5316\u52A8\
  \u8BCD\u5C06\u53D8\u91CF\u6CE8\u5165\u5230\u5B57\u7B26\u4E32\u4E2D\u3002\u683C\u5F0F\
  \u5316\u52A8\u8BCD\u662F\u683C\u5F0F\u5B57\u7B26\u4E32\u4E2D\u7684\u5360\u4F4D\u7B26\
  \uFF0C\u5E76\u88AB\u7ED9\u5B9A\u53D8\u91CF\u7684\u503C\u6240\u66FF\u6362\u3002\u4EE5\
  \u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\uFF1A."
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## 如何操作：
在 Go 中，字符串插值通常通过使用 `fmt` 包来实现，特别是 `Sprintf` 函数，它允许你通过指定格式化动词将变量注入到字符串中。格式化动词是格式字符串中的占位符，并被给定变量的值所替换。以下是如何使用它：

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // 使用 Sprintf 进行字符串插值
    message := fmt.Sprintf("Hello, my name is %s and I am %d years old.", name, age)
    fmt.Println(message) // 输出：Hello, my name is Jane and I am 28 years old.
}
```

注意，`%s` 用于字符串，而 `%d` 用于整数。`fmt` 包文档提供了不同数据类型的格式化动词的全面列表。

## 深入探讨
许多编程语言中都存在字符串插值的概念，尽管它们的语法和能力各不相同。在 Go 中，虽然 `fmt` 包的 `Sprintf` 函数是最常用的方法，但在简单的连接操作或在高度性能敏感的代码中，它可能并不总是最高效的。

`fmt` 包使用反射动态地在运行时解释变量的类型，这虽然灵活，但会带来开销。在性能至关重要的场景中，直接字符串连接或 `strings.Builder` 类型可能提供更好的替代方案。直接连接很直接，但在处理多个变量时可能变得难以管理。另一方面，`strings.Builder` 提供了一种更高效且可读的方式来在循环中或处理多个变量时构建复杂字符串：

```go
var sb strings.Builder
sb.WriteString("Hello, my name is ")
sb.WriteString(name)
sb.WriteString(" and I am ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" years old.")
message := sb.String()

fmt.Println(message) // 输出与之前相同
```

最终，选择 `fmt.Sprintf`、直接连接和 `strings.Builder` 之间的决策取决于你的应用程序的具体要求，如所构建字符串的复杂性和性能考虑因素。
