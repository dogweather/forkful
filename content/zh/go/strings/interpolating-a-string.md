---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:31.040700-07:00
description: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u4E00\u79CD\u6784\u9020\u5B57\u7B26\
  \u4E32\u7684\u65B9\u6CD5\uFF0C\u5B83\u53EF\u4EE5\u6574\u5408\u53D8\u91CF\uFF0C\u4F7F\
  \u5F97\u53EF\u4EE5\u52A8\u6001\u5730\u521B\u5EFA\u5B57\u7B26\u4E32\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u81EA\u5B9A\u4E49\u6D88\u606F\uFF0C\u6784\
  \u5EFA URLs\uFF0C\u521B\u5EFA SQL \u67E5\u8BE2\u7B49\uFF0C\u4F7F\u5F97\u4EE3\u7801\
  \u66F4\u53EF\u8BFB\u4E14\u6613\u4E8E\u7EF4\u62A4\u3002"
lastmod: '2024-03-13T22:44:47.124783-06:00'
model: gpt-4-0125-preview
summary: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u4E00\u79CD\u6784\u9020\u5B57\u7B26\
  \u4E32\u7684\u65B9\u6CD5\uFF0C\u5B83\u53EF\u4EE5\u6574\u5408\u53D8\u91CF\uFF0C\u4F7F\
  \u5F97\u53EF\u4EE5\u52A8\u6001\u5730\u521B\u5EFA\u5B57\u7B26\u4E32\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u81EA\u5B9A\u4E49\u6D88\u606F\uFF0C\u6784\
  \u5EFA URLs\uFF0C\u521B\u5EFA SQL \u67E5\u8BE2\u7B49\uFF0C\u4F7F\u5F97\u4EE3\u7801\
  \u66F4\u53EF\u8BFB\u4E14\u6613\u4E8E\u7EF4\u62A4\u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
---

{{< edit_this_page >}}

## 什么 & 为什么?

字符串插值是一种构造字符串的方法，它可以整合变量，使得可以动态地创建字符串。程序员这样做是为了自定义消息，构建 URLs，创建 SQL 查询等，使得代码更可读且易于维护。

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
