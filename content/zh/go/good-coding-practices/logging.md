---
title:                "日志记录"
aliases:
- /zh/go/logging/
date:                  2024-02-03T17:59:07.288697-07:00
model:                 gpt-4-0125-preview
simple_title:         "日志记录"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/logging.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

软件开发中的日志记录是指记录程序执行信息的过程，旨在跟踪其行为并诊断问题。程序员实现日志记录以监控软件性能、调试错误，并确保系统安全和合规，使其成为应用维护和分析的不可或缺的工具。

## 如何操作：

在 Go 中，可以使用标准库包 `log` 来实现日志记录。这个包提供了简单的日志记录功能，比如写入标准输出或文件。我们先从一个基础的例子开始，记录到标准输出：

```go
package main

import (
	"log"
)

func main() {
	log.Println("这是一个基础的日志条目。")
}
```

输出：
```
2009/11/10 23:00:00 这是一个基础的日志条目。
```

日志条目开头的时间戳是由 `log` 包自动添加的。接下来，让我们探索如何将日志记录到文件，而不是标准输出：

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("这条日志条目记录到文件中。")
}
```

现在，让我们实现一个更高级的用例：自定义日志格式。Go 允许您使用 `log.New()` 创建一个自定义的日志器：

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "自定义日志: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("这是一个自定义的日志消息。")
}
```

输出：
```
自定义日志: 2009/11/10 23:00:00 main.go:11: 这是一个自定义的日志消息。
```

这个例子以 "自定义日志: " 作为每条日志消息的前缀，并包括日期、时间和源文件位置。

## 深入探讨

Go 标准库的 `log` 包简单直接，适用于许多应用程序，但它缺少一些第三方日志库中找到的更复杂的功能，例如结构化日志、日志轮转和基于级别的日志。如 `zap` 和 `logrus` 这样的包提供了这些高级功能，并且因其性能和灵活性在 Go 社区中享有盛誉。

例如，结构化日志允许您以结构化格式（如 JSON）记录数据，这对于可能通过各种工具或服务分析日志的现代云基础应用尤其有用。`zap` 特别以其高性能和低分配开销而闻名，适用于速度和效率至关重要的应用程序。

从历史上看，自 Go 语言诞生以来，Go 中的日志记录已经发生了显著的演变。Go 的早期版本提供了我们在 `log` 包中看到的基本日志记录功能。然而，随着 Go 语言变得更加流行，用 Go 编写的应用程序的复杂性增加，社区开始开发更复杂的日志库以满足他们的需求。如今，尽管标准的 `log` 包仍然是简单应用程序的一个可行选项，但许多开发人员为了满足更复杂的日志记录需求而转向这些第三方解决方案。
