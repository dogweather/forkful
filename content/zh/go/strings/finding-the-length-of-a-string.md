---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:21.197982-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Go \u4E2D\uFF0C\u5B57\u7B26\u4E32\
  \u88AB\u89C6\u4E3A\u4E0D\u53EF\u53D8\u7684\u5B57\u8282\u5E8F\u5217\u3002\u60A8\u53EF\
  \u4EE5\u4F7F\u7528\u5185\u7F6E\u7684 `len()` \u51FD\u6570\u6765\u627E\u5230\u4E00\
  \u4E2A\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\uFF0C\u8BE5\u51FD\u6570\u8FD4\u56DE\u7684\
  \u662F\u5B57\u8282\u6570\uFF0C\u800C\u4E0D\u4E00\u5B9A\u662F\u5B57\u7B26\u6570\u3002\
  \u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T22:38:46.319571-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Go \u4E2D\uFF0C\u5B57\u7B26\u4E32\u88AB\
  \u89C6\u4E3A\u4E0D\u53EF\u53D8\u7684\u5B57\u8282\u5E8F\u5217\u3002\u60A8\u53EF\u4EE5\
  \u4F7F\u7528\u5185\u7F6E\u7684 `len()` \u51FD\u6570\u6765\u627E\u5230\u4E00\u4E2A\
  \u5B57\u7B26\u4E32\u7684\u957F\u5EA6\uFF0C\u8BE5\u51FD\u6570\u8FD4\u56DE\u7684\u662F\
  \u5B57\u8282\u6570\uFF0C\u800C\u4E0D\u4E00\u5B9A\u662F\u5B57\u7B26\u6570\u3002\u4EE5\
  \u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u7684\u65B9\u6CD5\uFF1A."
title: "\u67E5\u627E\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## 如何操作：
在 Go 中，字符串被视为不可变的字节序列。您可以使用内置的 `len()` 函数来找到一个字符串的长度，该函数返回的是字节数，而不一定是字符数。以下是如何使用它的方法：

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// 使用 len() 来找到字节长度
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("字节长度:", byteLength) // 输出：字节长度: 13

	// 为了准确获取字符串中的字符数或符文数
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("符文长度:", runeLength) // 输出：符文长度: 9
}
```
使用 `len()` 的第一种方法可能不会始终给出预期的结果，因为它计算的是字节。对于包含非 ASCII 字符（如 "世界"）的字符串来说，应该改用 `unicode/utf8` 包中的 `RuneCountInString` 来准确计算 Unicode 代码点。

## 深入探讨
在 Go 1 之前，没有严格的界定来处理字符串作为字节序列还是字符序列。Go 1 之后，采用 UTF-8 作为字符串的标准编码方案，这就需要更清晰的方法。对于 ASCII 字符串，`len()` 函数工作得很完美，其中字符用单个字节表示。然而，随着 Go 应用变得越来越全球化，以及对大量语言和字符集的支持需求增长，`len()` 的简单方法显示出了限制。

`utf8.RuneCountInString()` 的引入和使用，通过提供一种计算实际 Unicode 字符（在 Go 术语中是符文）的方法，回应了这些限制。这种方法确保了长度计算独立于 UTF-8 编码的具体情况，其中字符可能跨越多个字节。

一个替代的方法，更符合 Go 的并发性和效率精神，可能涉及将字符串视为符文的切片来遍历和操纵字符串。然而，这种方法需要一个转换步骤，并且并不立即解决 Unicode 的所有复杂性（例如，组合字符）。

总而言之，尽管 `len()` 适用于字节长度，并且对于 ASCII 文本来说效率很高，但对于全球兼容应用来说，`utf8.RuneCountInString()` 是一个更可靠的选择。然而，开发者被鼓励理解这些选择所涉及的性能和内存使用的权衡。
