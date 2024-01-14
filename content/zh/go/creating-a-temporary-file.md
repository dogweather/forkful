---
title:                "Go: 生成临时文件"
simple_title:         "生成临时文件"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

临时文件是在程序执行过程中创建的临时存储文件。它可以帮助我们在运行程序时，保存和处理临时数据，保证程序的稳定性和可靠性。创建临时文件也可以帮助我们在程序结束后自动清除不需要的文件，避免占用过多的磁盘空间。

## 如何创建

我们可以使用Go语言中的`ioutil.TempFile`函数来创建临时文件。在函数中，我们可以指定临时文件的存放位置和前缀，并可选择是否自动清除临时文件。下面是一个示例代码：

```Go
// 导入必要的包
import "io/ioutil"
import "os"

// 创建临时文件
tempFile, err := ioutil.TempFile("/path/to/directory", "prefix-")

// 检查错误
if err != nil {
    panic(err)
}

// 关闭临时文件
defer tempFile.Close()

// 使用临时文件做一些操作
// ...

// 手动清除临时文件
// ...

// 自动清除临时文件
defer os.Remove(tempFile.Name())

```

运行上面的代码，我们会在指定的路径下看到一个以`prefix-`开头的临时文件。

## 深入了解

除了使用`ioutil.TempFile`函数外，我们也可以使用系统调用来创建临时文件。在Linux系统中，我们可以使用`mktemp`命令来创建临时文件，并指定后缀、前缀和存放位置。下面是一个示例代码：

```Go
// 导入必要的包
import "os/exec"

// 创建临时文件
// 指定存放位置为当前目录，前缀为"prefix-"，后缀为".tmp"
tempFile, err := exec.Command("bash", "-c", "mktemp --tmpdir=. prefix-XXXXXX.tmp").Output()

// 检查错误
if err != nil {
    panic(err)
}

// 打印临时文件名
fmt.Printf("Temp file name: %s", string(tempFile))

```

运行上面的代码，我们会在当前目录下看到一个以`prefix-`开头，以`.tmp`结尾的临时文件。

## 参考链接

- [Go语言官方文档 - ioutil.TempFile](https://golang.org/pkg/io/ioutil/#TempFile)
- [Linux mktemp命令文档](https://www.lesstif.com/pages/viewpage.action?pageId=20775973)