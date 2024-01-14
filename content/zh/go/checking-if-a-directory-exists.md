---
title:    "Go: 检查目录是否存在"
keywords: ["Go"]
---

{{< edit_this_page >}}

# 为什么要检查目录是否存在？

在编程过程中，有时候我们需要检查某个目录是否存在，以便在后续的代码中做出相应的处理。这可以帮助我们避免出现出错的情况，并且可以保证程序的正常运行。

## 如何进行检查？

在Go语言中，我们可以使用os包中的`Stat()`函数来检查目录的存在性。下面是一个示例代码：

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // 定义要检查的目录
    dir := "/Users/username/go/src"

    // 使用Stat()函数检查目录是否存在
    _, err := os.Stat(dir)
    if err == nil {
        // 目录存在
        fmt.Println("目录存在！")
    } else if os.IsNotExist(err) {
        // 目录不存在
        fmt.Println("目录不存在！")
    } else {
        // 其他错误
        fmt.Println(err)
    }
}
```

上面的代码首先定义了要检查的目录，然后使用`Stat()`函数来检查目录的存在性。如果目录存在，则输出"目录存在！"，如果不存在，则输出"目录不存在！"。注意，在检查目录不存在的情况下，我们使用了`os.IsNotExist()`函数来判断具体的错误类型。

## 深入了解

在进行目录存在性检查时，我们还可以使用`os.IsExist()`函数来判断目录是否存在，它的返回值为布尔类型。另外，我们也可以使用`os.Getwd()`函数来获取当前工作目录，然后再判断是否为目标目录。

# 参考资料

- [Go语言官方文档 - os包](https://golang.org/pkg/os/)
- [Go语言中文网 - Os包文档](https://studygolang.com/pkgdoc)
- [Go语言中文网 - os包介绍及使用示例](https://studygolang.com/articles/3928)

# 参见

- [Go语言中文网 - 文件操作](https://studygolang.com/subject/22)
- [GopherCon 2018 - Validating Files And Directories In Go](https://www.youtube.com/watch?v=gcI-oK-iifg) (英文)