---
title:    "Go: 检查目录是否存在"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么要检查目录是否存在

检查一个目录是否存在，是一个常见的编程需求。在编程中，我们经常需要创建、修改或删除文件，而这些操作都需要先确定目录是否存在。

# 如何检查目录是否存在

在Go语言中，我们可以使用`os.Stat()`函数来检查目录是否存在。这个函数的返回值是一个`os.FileInfo`类型的对象，我们可以通过它的`IsDir()`方法来判断是否为目录。

```Go
func main() {
    // 定义一个目录路径
    dir := "my_directory"

    // 调用os.Stat()函数检查目录是否存在
    _, err := os.Stat(dir)

    // 判断错误类型，如果是目录不存在的错误，则输出"目录不存在"
    if os.IsNotExist(err) {
        fmt.Println("目录不存在")
    }
}
```

输出结果：

```
目录不存在
```

# 深入了解

除了使用`os.Stat()`函数外，我们还可以使用`os.IsDir()`函数来判断一个路径是否为目录。这个函数的返回值是一个布尔值，如果路径是目录，则返回`true`，否则返回`false`。

```Go
func main() {
    // 定义一个目录路径
    dir := "my_directory"

    // 调用os.IsDir()函数判断路径是否为目录
    isDir := os.IsDir(dir)

    // 如果是目录，则输出"这是一个目录"
    if isDir {
        fmt.Println("这是一个目录")
    }
}
```

输出结果：

```
这是一个目录
```

# 参考链接

- [Go语言标准库官方文档 - os包](https://golang.org/pkg/os/)
- [Go语言标准库官方文档 - filepath包](https://golang.org/pkg/filepath/)
- [Go语言标准库官方文档 - io/fs包](https://golang.org/pkg/io/fs/)
- [Go语言实战 - 检查文件或目录是否存在](https://github.com/astaxie/build-web-application-with-golang/blob/master/zh/02.2.md)

# 参见

- [GitHub - Go语言中文网](https://github.com/golang-china/golangdoc)
- [Go语言中文网](https://golang-china.org/)