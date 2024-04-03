---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:50.235666-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Go \u4E2D\uFF0C\u5199\u5165\u6587\
  \u672C\u6587\u4EF6\u662F\u7531 `os` \u548C `io/ioutil`\uFF08\u5BF9\u4E8E Go \u7248\
  \u672C <1.16\uFF09\u6216\u8005\u662F `os` \u548C `io` \u52A0\u4E0A `os` \u5305\uFF08\
  \u5BF9\u4E8E Go 1.16 \u53CA\u4EE5\u4E0A\u7248\u672C\uFF09\u6765\u5904\u7406\u7684\
  \uFF0C\u8FD9\u4F53\u73B0\u4E86 Go \u7684\u7B80\u6D01\u548C\u6548\u7387\u7684\u8BBE\
  \u8BA1\u54F2\u5B66\u3002\u65B0\u7684 API\u2026"
lastmod: '2024-03-13T22:44:47.166635-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\uFF0C\u5199\u5165\u6587\u672C\u6587\u4EF6\u662F\u7531 `os`\
  \ \u548C `io/ioutil`\uFF08\u5BF9\u4E8E Go \u7248\u672C <1.16\uFF09\u6216\u8005\u662F\
  \ `os` \u548C `io` \u52A0\u4E0A `os` \u5305\uFF08\u5BF9\u4E8E Go 1.16 \u53CA\u4EE5\
  \u4E0A\u7248\u672C\uFF09\u6765\u5904\u7406\u7684\uFF0C\u8FD9\u4F53\u73B0\u4E86 Go\
  \ \u7684\u7B80\u6D01\u548C\u6548\u7387\u7684\u8BBE\u8BA1\u54F2\u5B66\u3002\u65B0\
  \u7684 API \u4EE5\u66F4\u7B80\u5355\u7684\u9519\u8BEF\u5904\u7406\u63A8\u52A8\u4E86\
  \u66F4\u597D\u7684\u5B9E\u8DF5\u3002\u8BA9\u6211\u4EEC\u6DF1\u5165\u4E86\u89E3\u5982\
  \u4F55\u4F7F\u7528 Go \u7684 `os` \u5305\u521B\u5EFA\u548C\u5199\u5165\u6587\u672C\
  \u6587\u4EF6."
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

## 如何操作：
在 Go 中，写入文本文件是由 `os` 和 `io/ioutil`（对于 Go 版本 <1.16）或者是 `os` 和 `io` 加上 `os` 包（对于 Go 1.16 及以上版本）来处理的，这体现了 Go 的简洁和效率的设计哲学。新的 API 以更简单的错误处理推动了更好的实践。让我们深入了解如何使用 Go 的 `os` 包创建和写入文本文件。

首先，确保你的 Go 环境已经设置并准备就绪。然后，创建一个 `.go` 文件，例如 `writeText.go`，并在你的文本编辑器或 IDE 中打开它。

这是一个简单的示例，它将一个字符串写入名为 `example.txt` 的文件中：

```go
package main

import (
    "os"
    "log"
)

func main() {
    content := []byte("Hello, Wired readers!\n")

    // 创建或覆盖文件 example.txt
    err := os.WriteFile("example.txt", content, 0644)
    if err != nil {
        log.Fatal(err)
    }
}

```

当你使用 `go run writeText.go` 运行这段代码时，它会创建（或覆盖如果已存在）一个名为 `example.txt` 的文件，内容为 "Hello, Wired readers!"。

### 向文件追加内容
如果你想要追加内容怎么办？Go 也提供了灵活的处理方式：

```go
file, err := os.OpenFile("example.txt", os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0644)
if err != nil {
    log.Fatal(err)
}
defer file.Close()

if _, err := file.WriteString("Appending more text.\n"); err != nil {
    log.Fatal(err)
}
```

这个片段以追加模式打开 `example.txt`，写入额外的一行，并确保即使发生错误文件也能被正确关闭。

## 深入了解
Go 在文件处理方面的演变反映了其对代码简洁性和效率的更广泛承诺。早期版本更多地依赖于 `ioutil` 包，需要更多的冗长表达和略高的错误可能性。特别是从版本 1.16 开始，向 `os` 和 `io` 包增强功能的转变，展示了 Go 在简化文件操作方面的主动措施，鼓励更一致的错误处理，并使该语言更加容易上手。

尽管 Go 的内置库对许多用例已经足够，但在某些情形下，尤其是在进行更复杂的文件操作或在提供特定文件处理抽象的更大框架内工作时，可能会更偏好使用替代包或外部库。然而，对于直接的、简单的文件写入任务，标准库通常在 Go 编程中提供了最有效和最符合惯用法的途径。向更简单、更集中的文件操作 API 的过渡，不仅使 Go 代码更易于编写和维护，而且还强调了该语言的简洁、可读性和实用性哲学。
