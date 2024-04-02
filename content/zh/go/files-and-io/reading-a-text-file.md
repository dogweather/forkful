---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:02.672511-07:00
description: "\u5728 Go \u4E2D\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5305\u62EC\u8BBF\
  \u95EE\u548C\u68C0\u7D22\u5B58\u50A8\u5728\u78C1\u76D8\u4E0A\u7684\u6587\u4EF6\u5185\
  \u5BB9\uFF0C\u4EE5\u4FBF\u8FDB\u884C\u5904\u7406\u6216\u5206\u6790\u3002\u7A0B\u5E8F\
  \u5458\u7ECF\u5E38\u6267\u884C\u6B64\u64CD\u4F5C\u4EE5\u64CD\u7EB5\u6570\u636E\uFF0C\
  \u914D\u7F6E\u5E94\u7528\u7A0B\u5E8F\uFF0C\u6216\u8BFB\u53D6\u7A0B\u5E8F\u6267\u884C\
  \u7684\u8F93\u5165\uFF0C\u8FD9\u4F7F\u5176\u6210\u4E3A\u8F6F\u4EF6\u5F00\u53D1\u4E2D\
  \u7684\u57FA\u7840\u6280\u80FD\u3002"
lastmod: '2024-03-13T22:44:47.165355-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5305\u62EC\u8BBF\u95EE\
  \u548C\u68C0\u7D22\u5B58\u50A8\u5728\u78C1\u76D8\u4E0A\u7684\u6587\u4EF6\u5185\u5BB9\
  \uFF0C\u4EE5\u4FBF\u8FDB\u884C\u5904\u7406\u6216\u5206\u6790\u3002\u7A0B\u5E8F\u5458\
  \u7ECF\u5E38\u6267\u884C\u6B64\u64CD\u4F5C\u4EE5\u64CD\u7EB5\u6570\u636E\uFF0C\u914D\
  \u7F6E\u5E94\u7528\u7A0B\u5E8F\uFF0C\u6216\u8BFB\u53D6\u7A0B\u5E8F\u6267\u884C\u7684\
  \u8F93\u5165\uFF0C\u8FD9\u4F7F\u5176\u6210\u4E3A\u8F6F\u4EF6\u5F00\u53D1\u4E2D\u7684\
  \u57FA\u7840\u6280\u80FD\u3002"
title: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6"
weight: 22
---

## 什么 & 为什么？

在 Go 中读取文本文件包括访问和检索存储在磁盘上的文件内容，以便进行处理或分析。程序员经常执行此操作以操纵数据，配置应用程序，或读取程序执行的输入，这使其成为软件开发中的基础技能。

## 如何操作：

在 Go 中读取文本文件可以通过几种方式完成，但其中一种最直接的方法是使用 `ioutil` 包。这是一个基本示例：

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
)

func main() {
    content, err := ioutil.ReadFile("example.txt")
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(string(content))
}
```

假设 `example.txt` 包含 "Hello, Go!"，此程序将输出：

```
Hello, Go!
```

然而，从 Go 1.16 开始，`ioutil` 包已被弃用，建议改用 `os` 和 `io` 包。以下是使用这些包实现相同功能的方法：

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```

这种方法不仅更现代，而且还支持更大的文件，因为它逐行读取文件，而不是一次性将整个内容加载到内存中。

## 深入探讨：

Go 在文件操作（包括从文件中读取）的处理反映了该语言的简单和效率的理念。起初，`ioutil` 包提供了直接的文件操作方法。然而，随着 Go 标准库的改进以及对更明确的错误处理和资源管理的转向，`os` 和 `io` 包已成为处理文件的首选替代品。

这些更改强调了 Go 对性能和安全性的承诺，特别是在避免因一次性加载大文件而可能出现的内存问题上。为逐行读取文件引入的 `bufio.Scanner` 方法凸显了该语言的适应性和对现代计算挑战（如处理大型数据集或流数据）的关注。

虽然有可用于 Go 文件处理的外部库，但标准库的能力通常是足够的，也是因其稳定性和性能而被优先考虑。这确保了 Go 开发人员可以有效地管理文件操作，而无需依赖额外的依赖性，这与该语言整体的简约主义精神和构建高效、可靠软件的设计理念相一致。
