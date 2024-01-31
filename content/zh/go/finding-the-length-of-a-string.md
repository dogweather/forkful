---
title:                "获取字符串的长度"
date:                  2024-01-20T17:47:43.084925-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么及为什么？)
计算字符串长度就是确定一个字符串中有多少字符。程序员这么做通常是为了验证输入、设置文本界面元素或进行诸如截断、比较等操作。

## How to: (如何操作)
```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// 示例字符串
	str := "Hello, 世界"
	
	// 使用 len() 获取字节长度
	fmt.Println("Bytes:", len(str)) // 输出字节长度
	
	// 使用 utf8.RuneCountInString() 获取字符长度
	charLength := utf8.RuneCountInString(str)
	fmt.Println("Characters:", charLength) // 输出字符长度
}
```
示例输出：
```
Bytes: 13
Characters: 9
```

## Deep Dive (深入探讨)
字符串长度可以按字节长度和字符长度来计算。Go 使用 UTF-8 编码字符串，所以一个字符可能不只一个字节。len() 函数返回的是字节长度，而不是字符数。历史上，由于 ASCII 编码的使用，一个字符等于一个字节，但 UTF-8 的出现打破了这一束缚。如果要处理国际化的文本，需要用 `utf8` 包中的方法来正确计算字符数量。替代方法包括使用 `range` 循环计算字符，但 `utf8.RuneCountInString()` 既简洁又高效。

## See Also (另请参阅)
- [Go 字符串处理指南](https://blog.golang.org/strings)
- [Go `len` 函数官方文档](https://pkg.go.dev/builtin#len)
- [Unicode 和 UTF-8 介绍](https://unicodebook.readthedocs.io/guess_encoding.html)
