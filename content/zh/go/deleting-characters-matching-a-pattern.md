---
title:    "Go: 删除与模式匹配的字符"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么要删除符合模式的字符?

Go语言提供了一种非常方便的方法来删除字符串中符合特定模式的字符。这对于需要对字符串做一些特殊处理的情况非常有用。

## 如何做?

首先，我们需要导入`strings`包来使用字符串相关的函数。然后，我们可以使用`strings.ReplaceAll`函数来删除符合模式的字符。下面是一个简单的例子:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Hello Go lovers!"
    newStr := strings.ReplaceAll(str, "o", "")
    fmt.Println(newStr)
}
```

输出将会是 `Hell lovers!`，我们可以看到所有的`o`字符都被删除了。除了单个字符外，我们也可以删除指定的词语，只需要将第二个参数改为词语即可。

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "I love Go programming language!"
    newStr := strings.ReplaceAll(str, "Go programming", "")
    fmt.Println(newStr)
}
```

输出则是 `I love language!`。除了`strings.ReplaceAll`函数外，我们也可以使用`strings.Replace`函数来指定替换次数。更多关于字符串操作的相关函数，请参考Go语言官方文档。

## 深入了解

除了使用`strings.ReplaceAll`函数外，我们也可以使用正则表达式来删除符合特定模式的字符串。Go语言提供了`regexp`包来支持正则表达式操作。下面是一个使用正则表达式删除字符串的例子:

``` Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    str := "123abc456def789ghi"
    pattern := "[0-9]+"
    regex := regexp.MustCompile(pattern)
    newStr := regex.ReplaceAllString(str, "")
    fmt.Println(newStr)
}
```

输出为 `abcdefghi`，我们可以看到所有的数字都被成功删除了。除了删除外，我们也可以使用正则表达式来进行替换等操作。更多关于`regexp`包的使用，请参考Go语言官方文档。

## 同样值得一看

- [Go语言官方文档](https://golang.org/doc/)
- [Go语言中文网](https://studygolang.com/)
- [深入理解Go语言](https://chai2010.cn/advanced-go-programming-book/)