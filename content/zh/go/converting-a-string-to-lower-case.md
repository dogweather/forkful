---
title:    "Go: 将字符串转换为小写"
keywords: ["Go"]
---

{{< edit_this_page >}}

## 为什么要将字符串转换为小写？

在编程中，字符串是一种常见的数据类型，在某些情况下，我们需要将字符串转换为小写字母。这可能是为了比较字符串，或者是为了满足某些特定的格式要求。在Go语言中，我们可以很容易地将字符串转换为小写，下面将介绍具体的方法。

## 如何进行字符串转换为小写？

首先，我们需要使用语言内置的ToLower函数来实现字符串转换为小写。下面是一个例子：

```Go
package main

import "fmt"
import "strings"

func main() {
	// 创建一个字符串变量
	var str string = "Hello World!"

	// 使用ToLower函数将字符串转换为小写
	str = strings.ToLower(str)

	// 打印转换后的字符串
	fmt.Println(str)
}
```

该程序的输出结果为：

```
hello world!
```

如上所示，通过使用ToLower函数，我们可以轻松地将字符串转换为小写。另外，如果我们想要将字符串的开始部分转换为大写，可以使用ToUpper函数。

除了使用内置函数之外，我们还可以使用循环来实现字符串转换为小写。下面是另一个示例：

```Go
package main

import (
	"fmt"
	"unicode"
)

func main() {
	// 创建一个字符串变量
	var str string = "Hello World!"

	// 使用循环遍历字符串中的每个字符
	for _, c := range str {
		// 使用unicode包中的ToLower函数将每个字符转换为小写
		lower := unicode.ToLower(c)
		// 打印转换后的字符
		fmt.Printf("%c", lower)
	}
}
```

该程序的输出结果仍然是：

```
hello world!
```

## 深入了解字符串转换为小写

在Go语言中，字符串是不可变的，也就是说，我们无法直接在原有的字符串上进行转换。所以，每次转换都会返回一个新的字符串。另外，需要注意的是，在使用循环进行字符串转换时，需要使用unicode包中的相关函数，而不能直接使用字符串函数。

## 参考链接

- [Go语言中ToLower函数的用法](https://golang.org/pkg/strings/#ToLower)
- [Go语言中unicode包的文档](https://golang.org/pkg/unicode/)