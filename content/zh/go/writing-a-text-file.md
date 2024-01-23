---
title:                "编写文本文件"
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
写文本文件让数据持久化。程序员这么做可存配置、日志或其他数据，方便读写和共享。

## How to: 怎么做
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	file, err := os.Create("example.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()

	_, err = file.WriteString("Hello, Go!\n")
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println("文件写入成功")
}
```
输出：
```
文件写入成功
```

## Deep Dive: 深入了解
写文件在计算机历史上很古老，像UNIX早期就有。Go的 `os` 包提供了这功能，有多种方式，`io.WriteString` 或 `fmt.Fprintln` 。若关注性能，可考虑缓冲写入(`bufio`)。

## See Also: 参考链接
- Go文档: [os package](https://golang.org/pkg/os/)
- Go博客: [Defer, Panic and Recover](https://blog.golang.org/defer-panic-and-recover)
