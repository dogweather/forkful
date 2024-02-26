---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:51.692282-07:00
description: "\u5728 Go \u4E2D\u521B\u5EFA\u4E00\u4E2A\u4E34\u65F6\u6587\u4EF6\u53EF\
  \u4EE5\u751F\u6210\u4E00\u4E2A\u8BBE\u8BA1\u7528\u4E8E\u77ED\u671F\u4F7F\u7528\u7684\
  \u975E\u6301\u4E45\u6027\u6587\u4EF6\uFF0C\u4E3B\u8981\u7528\u4E8E\u5B58\u50A8\u4E34\
  \u65F6\u6570\u636E\u6216\u534F\u52A9\u6279\u5904\u7406\u4F5C\u4E1A\u7B49\u4EFB\u52A1\
  \u3002\u7A0B\u5E8F\u5458\u5229\u7528\u8FD9\u4E00\u529F\u80FD\u6765\u5B89\u5168\u5730\
  \u5904\u7406\u6570\u636E\uFF0C\u800C\u65E0\u9700\u5F71\u54CD\u6C38\u4E45\u6587\u4EF6\
  \u7CFB\u7EDF\u6216\u9700\u8981\u624B\u52A8\u6E05\u7406\u3002"
lastmod: '2024-02-25T18:49:44.800075-07:00'
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\u521B\u5EFA\u4E00\u4E2A\u4E34\u65F6\u6587\u4EF6\u53EF\u4EE5\
  \u751F\u6210\u4E00\u4E2A\u8BBE\u8BA1\u7528\u4E8E\u77ED\u671F\u4F7F\u7528\u7684\u975E\
  \u6301\u4E45\u6027\u6587\u4EF6\uFF0C\u4E3B\u8981\u7528\u4E8E\u5B58\u50A8\u4E34\u65F6\
  \u6570\u636E\u6216\u534F\u52A9\u6279\u5904\u7406\u4F5C\u4E1A\u7B49\u4EFB\u52A1\u3002\
  \u7A0B\u5E8F\u5458\u5229\u7528\u8FD9\u4E00\u529F\u80FD\u6765\u5B89\u5168\u5730\u5904\
  \u7406\u6570\u636E\uFF0C\u800C\u65E0\u9700\u5F71\u54CD\u6C38\u4E45\u6587\u4EF6\u7CFB\
  \u7EDF\u6216\u9700\u8981\u624B\u52A8\u6E05\u7406\u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么和为什么？

在 Go 中创建一个临时文件可以生成一个设计用于短期使用的非持久性文件，主要用于存储临时数据或协助批处理作业等任务。程序员利用这一功能来安全地处理数据，而无需影响永久文件系统或需要手动清理。

## 如何操作：

在 Go 中，`ioutil` 包原本提供了创建临时文件的实用程序。然而，Go 1.16 推广了将 `os` 和 `io/ioutil` 包的功能移到更有组织的位置。现在，优先选择 `os` 和 `io` 包来处理临时文件。

以下是创建、写入和删除临时文件的分步指南：

1. **创建一个临时文件：**

使用 `os.CreateTemp` 函数，你可以创建一个临时文件。如果不指定目录，它会使用操作系统的默认临时文件夹。

```go
package main

import (
    "io/ioutil"
    "log"
    "os"
)

func main() {
    tmpFile, err := ioutil.TempFile("", "example.*.txt")
    if err != nil {
        log.Fatal(err)
    }
    log.Printf("创建了临时文件：%s\n", tmpFile.Name())

    defer os.Remove(tmpFile.Name()) // 清理
}
```

2. **写入临时文件：**

可以通过 `Write` 方法或 `io` 或 `bufio` 包中的其他写入函数来完成写入。

```go
_, err = tmpFile.Write([]byte("Hello, World!"))
if err != nil {
    log.Fatal(err)
}
```

3. **从临时文件读取：**

读取类似地进行，使用文件的 `Read` 方法，或是使用 `io` 或 `bufio` 包中的工具。

```go
data, err := ioutil.ReadFile(tmpFile.Name())
if err != nil {
    log.Fatal(err)
}
log.Printf("读取的数据：%s\n", string(data))
```

4. **删除临时文件：**

虽然在创建阶段的 `defer os.Remove(tmpFile.Name())` 语句确保了程序终止后将删除临时文件，但也可以根据需要管理显式删除。

示例输出：
```
2023/04/01 15:00:00 创建了临时文件：/tmp/example.123456.txt
2023/04/01 15:00:00 读取的数据：Hello, World!
```

## 深入探究

Go 处理临时文件的机制已经发展。最初，创建临时文件主要由现已弃用的 `ioutil.TempFile` 函数管理，这反映了软件开发中朝向更安全、更高效的文件处理实践的更广泛趋势。随着 Go 1.16 将这些功能整合到 `os` 和 `io` 包中，标志着向简化语言的标准库和鼓励使用更统一、更协同的 API 方面的更广泛推动。

虽然使用临时文件是编程中常见且通常是必要的做法，但重要的是要注意，如果过多地依赖它们来存储大量数据或长期任务，可能会导致性能问题。此外，当临时文件的创建没有得到严格控制或它们没有得到充分清理时，可能会导致资源泄露，从而负面影响文件系统。在需要持久存储或处理大量数据流的场景中，数据库或内存数据存储等替代方案通常提供更好的性能和可靠性，与临时文件相比。
