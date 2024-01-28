---
title:                "从字符串中移除引号"
date:                  2024-01-26T03:39:49.145769-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

从字符串中去除引号意味着摆脱那些包裹实际文本的烦人的双引号或单引号字符。我们这样做是为了清理数据，防止解析错误，或者为进一步处理文本而准备，不带额外的引号标记。

## 如何操作：

以下是在Go中将引号丢弃的简单方法：

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Hello, World!\""
	fmt.Println("Original:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Unquoted:", unquotedString)
}
```

输出将会是这样，引号全部消失了：

```
Original: "Hello, World!"
Unquoted: Hello, World!
```

## 深入探究

回到过去，当数据格式和交换没有标准化时，字符串中的引号可能会造成混乱。它们仍然会，尤其是在JSON中或者当将字符串推入数据库时。Go的`strings`包装载了一个`Trim`函数，它不仅可以消除空白，还可以消除任何你不喜欢的字符。

为什么不用Regex？嗯，对于简单的任务，`Trim`更快，但如果你的字符串在奇怪的地方与引号捉迷藏，regex可能是你的重型火炮：

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

这就像在剪刀和链锯之间选择；选择适合工作的工具。

## 另请参见

了解更多关于`strings`包及其强大工具：
- [字符串包](https://pkg.go.dev/strings)

在Go中使用正则表达式的威力：
- [正则表达式包](https://pkg.go.dev/regexp)

想要深入了解字符串修剪的哲学？
- [Trim方法](https://blog.golang.org/strings)
