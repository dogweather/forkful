---
title:                "Go: 将字符串转换为小写."
simple_title:         "将字符串转换为小写."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么
转换字符串为小写只需要几行简单的代码，但它可以帮助我们处理文本数据，让它们更易于比较和分析。

## 如何做
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hello World"
	fmt.Println("原字符串：", str)
	strToLower := strings.ToLower(str)
	fmt.Println("转换为小写：", strToLower)
}
```
```
原字符串： Hello World
转换为小写： hello world
```

## 深入了解
在Go语言中，我们可以使用内置的strings包中的ToLower()函数来将字符串转换为小写。它会返回一个新的字符串，不会改变原来的字符串。这个函数会将字符串中的所有字母都转换为小写格式，并返回转换后的字符串。

除了ToLower()函数，还有一个ToUpper()函数，用来将字符串转换为大写。这两个函数都可以帮助我们快速处理字符串，让它们符合我们的需求。

## 参考链接
- [Go语言教程-字符串转换大小写](https://www.runoob.com/go/go-strings.html)
- [Strings包文档](https://golang.org/pkg/strings/)
- [更多Go语言教程](https://www.runoob.com/go/go-tutorial.html)

## 相关阅读