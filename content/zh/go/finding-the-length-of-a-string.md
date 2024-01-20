---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为何？ 
- 字符串长度是指字符串包含的字符个数。程序员因需要对数据大小进行控制和验证，数据截取，数据分段和搜索，常会需要获取字符串长度。

## 怎么做？
在Go中获取字符串长度，你只需要使用内置函数 `len()`。

```Go
package main
import "fmt"

func main() {
    str := "导入fmt"
    fmt.Println(len(str))
}
```

在上面的例子中，`len`函数会返回字符串`str`的长度，该长度为字符串中的字节个数。

## 深度剖析
- 关于获取字符串长度，Go有一些设计决策需要了解。`len()`函数返回的是字符串的字节数，而不是字符数。这是因为对于UTF-8编码的字符串，一个字符可能包含多个字节。所以，如果你的字符串包含非ASCII字符，则`len()`函数返回的可能不是你预期的字符数。

```Go
  package main
  import "fmt"

  func main() {
      str := "你好世界"
      fmt.Println(len(str))
  }
```
答案是12，而不是4，因为每个汉字字符在UTF-8中是3字节。

- 处理替代方法：`unicode/utf8`包的`RuneCountInString`函数可以返回字符串的字符数：
```Go
package main
import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "你好世界"
	fmt.Println(utf8.RuneCountInString(str))
}
```
这将返回4。

## 提示
如果你有兴趣进一步研究，你可以查看以下链接：
- 在Go中处理Unicode：[Go Blog：Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
- Go设计之初的一些决定：[Go at Google: Language Design in the Service of Software Engineering](https://talks.golang.org/2012/splash.article)