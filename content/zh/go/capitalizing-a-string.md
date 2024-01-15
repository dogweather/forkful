---
title:                "字符串大写化"
html_title:           "Go: 字符串大写化"
simple_title:         "字符串大写化"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，经常会遇到需要将字符串中的首字母大写的情况。这样做可以使字符串的格式更统一，更易于阅读和理解。

## 如何操作

要在Go中将字符串的首字母大写，可以使用内置的strings包中的Title函数。以下是一个示例代码和输出：

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hello world"
    changedStr := strings.Title(str)
    fmt.Println(changedStr)
}

// Output: Hello World
```

## 深入了解

除了Title函数外，还可以使用unicode包来操作字符串的首字母大小写。这里需要用到unicode包中的ToTitle和ToTitleSpecial函数。下面是一个示例代码和输出，展示了如何将字符串的第一个字母大写，同时保持其他字母小写：

```Go
package main

import (
    "fmt"
    "unicode"
)

func main() {
    str := "hello WorLD"
    first := str[0:1]
    rest := str[1:]
    changedStr := string(unicode.ToTitle(rune(first[0]))) + rest
    fmt.Println(changedStr)
}

// Output: Hello world
```

## 参考资料

- [Go官方文档：strings包](https://golang.org/pkg/strings/)
- [Go官方文档：unicode包](https://golang.org/pkg/unicode/)