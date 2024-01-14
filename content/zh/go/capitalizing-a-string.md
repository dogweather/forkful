---
title:                "Go: 将字符串的首字母大写"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

无论是初学者还是经验丰富的Go程序员，很多时候都会遇到需要将字符串中的字母大写的情况。这可能是因为你需要输出一个规范的字符串格式，或者是为了实现某些功能。无论什么原因，掌握如何将字符串中的字母大写是非常重要的。

## 怎么做

要将字符串中的字母大写，我们可以使用内置的strings包中的ToUpper函数。这个函数接收一个字符串并将其中的字母全部转换为大写形式。下面是一个简单的例子：

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "hello world"
	newS := strings.ToUpper(s)
	fmt.Println(newS)
}
```

运行结果为：

```Go
HELLO WORLD
```

## 深入学习

如果你想更深入地了解如何将字符串中的字母大写，可以查看Golang官方文档中关于strings包和ToUpper函数的介绍。这些资源都可以帮助你理解内置函数的原理，并且进一步掌握如何在实际项目中应用这些知识。

## 参考资料

- [Golang官方文档](https://golang.org/doc/)
- [strings package](https://golang.org/pkg/strings/)
- [ToUpper function](https://golang.org/pkg/strings/#ToUpper)

## 链接

请查看本文中提到的所有参考资料。