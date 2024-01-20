---
title:                "检查目录是否存在"
date:                  2024-01-20T14:56:24.166656-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (什么与为什么？)

检查目录是否存在是验证特定路径下文件夹是否存在的过程。程序员这么做是为了避免在执行文件操作时出现错误，比如尝试访问或创建一个已经存在或不存在的目录。

## How to: (如何操作：)

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	exists, err := directoryExists("/path/to/directory")
	if err != nil {
		fmt.Println("Error checking the directory:", err)
	} else if exists {
		fmt.Println("Directory exists.")
	} else {
		fmt.Println("Directory does not exist.")
	}
}

// directoryExists checks if a directory exists at the given path
func directoryExists(path string) (bool, error) {
	info, err := os.Stat(path)
	if os.IsNotExist(err) {
		return false, nil
	}
	return info.IsDir(), err
}
```

输出取决于指定路径的状态。可能是：

```
Directory exists.
```

或者：

```
Directory does not exist.
```

## Deep Dive (深入了解)

在Go的历史中，`os.Stat` 和 `os.IsNotExist` 函数常用于检查文件或目录是否存在。这种方法比尝试打开文件做操作更有效且明了。备选方法包括使用 `os.Open` 和 `os.Mkdir`，但它们常常用在需要对文件进行更多操作时。

实现方面，当 `os.Stat` 返回 `nil` 错误时，我们通常认为文件或目录存在。但这还不够，需要检查返回的 `FileInfo` 对象来确定路径指向的是不是一个目录 `info.IsDir()`。这种方法处理了一些边缘案例，如当路径指向文件而非目录时。

## See Also (另请参见)

- [os.Stat documentation](https://pkg.go.dev/os#Stat)
- [os.IsNotExist documentation](https://pkg.go.dev/os#IsNotExist)
- [File and Directory Operations in Go](https://go.dev/doc/articles/wiki/)
- [Effective Go](https://go.dev/doc/effective_go)