---
title:                "字符串首字母大写"
date:                  2024-01-19
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么与为什么？)
将字符串变为大写意味着把所有字母都转换为它们的大写形式。程序员这样做主要是为了统一数据格式或进行不区分大小写的比较。

## How to: (如何做：)
Go提供了一个标准库`strings`，其中包含一个ToUpper函数，用于将字符串转换成大写。让我们来看看怎么用。

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hello, 世界"
	upperStr := strings.ToUpper(str)
	fmt.Println(upperStr) // 输出: HELLO, 世界
}
```

## Deep Dive (深入探索)
在Go语言的早期版本中，也有将字符串变为大写的需求，但随着国际化的增长，处理各种语言的大写转换变得复杂。`strings.ToUpper`不仅对ASCII码有效，它兼顾Unicode字符，使得它可以处理包括汉字在内的世界上大多数写作系统。

替代方案中，如果你关心性能或特定场景，你可能会编写自定义的大写转化函数。例如，使用`unicode`标准库中的`To`函数可以更细粒度地控制转换过程。

实现细节上，`ToUpper`函数内部使用`unicode.SimpleFold`函数，遍历每个字符，查找它的大小写对应形式。

## See Also (另请参阅)
- Go标准库文档中的strings包：[strings package](https://pkg.go.dev/strings)
- Unicode标准库文档：[unicode package](https://pkg.go.dev/unicode)
