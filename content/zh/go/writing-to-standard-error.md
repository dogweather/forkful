---
title:                "写入标准错误"
date:                  2024-01-19
simple_title:         "写入标准错误"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为何？
在编程时，输出错误到标准错误（stderr）可以让你的错误信息不混在标准输出中。这样做有助于调试，并支持用户将程序的输出和错误流向不同的地方。

## How to: 如何操作：
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := fmt.Errorf("发生了一个错误")
	fmt.Fprintf(os.Stderr, "错误信息：%v\n", err)
}
```
Sample output:
```
错误信息：发生了一个错误
```
## Deep Dive: 深入探讨
在UNIX和类UNIX系统中，标准错误是一个输出流，独立于标准输出流（stdout）。将错误信息写入stderr而非stdout是为了能够使用管道和重定向操作符分开处理它们。Go语言中，可以使用 `os` 包中的 `Stderr` 值来实现。尽管 `log` 包提供了默认写入stderr的功能，直接使用 `os.Stderr` 是更低级的操作。

## See Also: 更多信息
- Go语言官方文档：[os package](https://pkg.go.dev/os#pkg-variables)
- Unix哲学：[The Unix Programming Environment](http://catb.org/~esr/writings/taoup/html/)
- 关于日志记录的最佳实践：[Log package](https://pkg.go.dev/log)
