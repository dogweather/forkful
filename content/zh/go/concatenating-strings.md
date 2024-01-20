---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么和为什么？
字符串连接是把两个或更多字符串的值结合起来形成一个新的单一字符串的过程。程序员之所以这么做，是因为他们需要动态地创建或改变字符串。

## 怎么做：
Go语言中最简单的连接字符串的方法就是使用加号（+）。诸如：
```Go
package main
import "fmt"

func main() {
    str1 := "你好,"
    str2 := "世界!"
    result := str1 + str2
    fmt.Println(result)
}
```
运行结果为：
```Go
你好,世界!
```
## 深入了解
从历史角度来看，字符串连接在编程语言发展的早期就已经存在，至今仍然是一项基础技术。在Go中，除了上述的加号，我们还有其他方式进行字符串连接，例如 `fmt.Sprintf` ， `strings.Join` 方法，也可以使用 `bytes.Buffer` 或 `strings.Builder`。

`fmt.Sprintf` 是一个格式化的字符串拼接方法：
```Go
package main
import (
    "fmt"
)

func main() {
	str1 := "你好,"
	str2 := "世界!"
	result := fmt.Sprintf("%s%s", str1, str2)
	fmt.Println(result)
}
```

`strings.Join`可以将字符串数组连接到一起：
```Go
package main
import (
    "fmt"
    "strings"
)

func main() {
    messages := []string{"你好,", "世界!"}
    fmt.Println(strings.Join(messages, ""))
}
```

考虑到效率，当需要连接的字符串特别多的时候，使用 `bytes.Buffer` 或 `strings.Builder` 更加建议：
```Go
package main
import (
	"bytes"
	"fmt"
)

func main() {
	var b bytes.Buffer

	for i := 0; i < 100; i++ {
		fmt.Fprintf(&b, "%d,", i)
	}

	output := b.String()
	fmt.Println(output)
}
```

## 另请参见:
* Go官方文档: [字符串处理](https://golang.org/pkg/strings/)
* Go官方博客: [Go字符串连接最佳实践](https://go.dev/blog/strings)
* 在线Go编程教程: [Go字符串连接](https://gobyexample.com/string-formatting)