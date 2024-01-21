---
title:                "阅读文本文件"
date:                  2024-01-20T17:54:15.492669-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
读取文本文件就是将文件内容加载进内存。程序员这样做以便处理数据、配置、日志等。

## How to: (如何操作：)
简单读取文件内容：

```Go
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
示例输出：
```
Hello, 这是你的文本内容！
```

逐行读取文件内容：

```Go
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

## Deep Dive (深入探究)
历史背景：`ioutil.ReadFile` 是旧版指令，但简单好用。Go 1.16 引入了 `os.ReadFile` 和 `io/fs` 包，提供更好的错误处理。

替代方案：可以用 `os.Open` 加 `bufio.Scanner` 逐行读入，适合大文件；或者 `ioutil.ReadFile` 一次性读入小文件。

执行细节：考虑错误处理很重要，例如文件不存在或权限不够。关闭文件 (`file.Close()`) 是个好习惯，防止内存泄漏。

## See Also (另请参阅)
- Go by Example: Reading Files - https://gobyexample.com/reading-files
- Go Doc: ioutil package - https://pkg.go.dev/io/ioutil
- Go Doc: os package - https://pkg.go.dev/os
- Go Doc: bufio package - https://pkg.go.dev/bufio