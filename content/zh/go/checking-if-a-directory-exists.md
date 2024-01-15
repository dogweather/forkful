---
title:                "检查目录是否存在"
html_title:           "Go: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么会有目录检查？

在编写代码时，有时会遇到需要检查特定目录是否存在的情况，这可以帮助我们避免出现错误并增加我们代码的健壮性。

## 如何进行目录检查？

通过使用内置的 `os.Stat` 函数和 `os.IsNotExist` 方法，我们可以轻松地检查目录是否存在。下面是一个例子：

```Go
// 导入必要的包
import (
    "fmt"
    "os"
)

// 定义需要检查的目录
dir := "sample_directory"

// 使用 os.Stat 函数检查目录
if _, err := os.Stat(dir); os.IsNotExist(err) {
    // 如果目录不存在，则输出提示信息
    fmt.Printf("目录 %s 不存在。\n", dir)
} else {
    // 如果目录存在，则输出成功信息
    fmt.Printf("目录 %s 存在。\n", dir)
}

// 输出如下结果：
// 目录 sample_directory 不存在。
```

## 深入了解目录检查

`os.Stat` 函数返回了一个 `os.FileInfo` 对象，这个对象包含了文件或目录的很多信息，比如文件大小、修改时间等。通过这个对象，我们可以进一步了解目录的更多细节。

## 参考链接

- [Go官方文档 - os.Stat 函数](https://golang.org/pkg/os/#Stat)
- [Go官方文档 - os.FileInfo 对象](https://golang.org/pkg/os/#FileInfo)
- [Go语言中文网 - 目录不存在时使用os.IsNotExist函数判断](https://studygolang.com/articles/7801)