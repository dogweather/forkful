---
title:                "将字符串大写"
html_title:           "Go: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？
字符串大写化是将所有字符都改成大写。程序员这么做主要是为了使数据具有一致性，便于比较和排序.

## 如何做：
在Go语言中，`strings`包提供了`ToUpper`函数来实现这一功能：

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hello, world!"
	fmt.Println(strings.ToUpper(str))
}
```
运行结果输出为：
```
HELLO, WORLD!
```
## 深入挖掘
1. 历史背景：在早期的编程语言如COBOL中，只支持大写字母。这也是今天在很多编程语言中我们提供这类函数的历史原因.
2. 替代方式：除了使用内置函数，你也可以使用循环遍历字符串中的每个字符，然后使用`unicode`包中的`ToUpper`函数将其转化为大写.
3. 实现细节：`ToUpper`函数在内部遍历字符串中的每个字符，然后使用`unicode`包中的`ToUpper`函数将其转化为大写。有了这个背景知识，你就可以理解为什么`ToUpper`函数可以处理包括Unicode在内的所有文本。

## 另请参阅
- Go语言标准库文档：https://golang.org/pkg/
- Unicode字符数据库：https://www.unicode.org/charts/
- Go语言字符串和字符的介绍: https://studygolang.com/articles/11802
- Go语言strings包的源码: https://github.com/golang/go/tree/master/src/strings