---
title:                "下载网页"
date:                  2024-01-20T17:44:02.250696-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"

category:             "Go"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
下载网页就是将网页内容从服务器获取并保存到本地。程序员这么做是为了数据分析、备份内容或离线查看。

## 如何做：
```Go
package main

import (
    "fmt"
    "io"
    "net/http"
    "os"
)

func main() {
    // 网页URL
    url := "http://example.com"

    // 发起请求
    resp, err := http.Get(url)
    if err != nil {
        fmt.Println("http.Get 错误:", err)
        return
    }
    defer resp.Body.Close()

    // 创建文件保存网页内容
    outFile, err := os.Create("example.html")
    if err != nil {
        fmt.Println("os.Create 错误:", err)
        return
    }
    defer outFile.Close()

    // 复制响应正文到文件
    _, err = io.Copy(outFile, resp.Body)
    if err != nil {
        fmt.Println("io.Copy 错误:", err)
        return
    }

    fmt.Println("下载完成")
}
```
执行上述代码，会看到输出「下载完成」，同时当前目录下会出现一个名为`example.html`的文件，内容是`http://example.com`的HTML代码。

## 深入探究
回顾历史，早期下载网页常通过命令行工具如`wget`实现。Go语言以其简洁和并发的特性，逐渐成为网络操作的热门选择。除了`net/http`标准库，还有如`curl`的Go实现版本，它们提供更多功能。在解决内存泄露、提高效率等实现细节方面，使用`defer`关闭响应体和文件句柄至关重要。在大规模爬虫中，管理和优化连接池、处理异常和数据编码是常见的深层话题。

## 参考链接
- Go `net/http` 文档: [https://pkg.go.dev/net/http](https://pkg.go.dev/net/http)
- Go by Example 教程: [https://gobyexample.com](https://gobyexample.com)
