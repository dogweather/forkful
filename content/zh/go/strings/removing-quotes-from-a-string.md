---
aliases:
- /zh/go/removing-quotes-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:26.647024-07:00
description: "\u5728 Go \u4E2D\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7\
  \u662F\u6307\u53BB\u9664\u7ED9\u5B9A\u5B57\u7B26\u4E32\u9996\u5C3E\u7684\u5F15\u53F7\
  (`\"` \u6216 `'`)\u3002\u7A0B\u5E8F\u5458\u5E38\u5E38\u9700\u8981\u6267\u884C\u8FD9\
  \u4E00\u4EFB\u52A1\u4EE5\u6E05\u6D17\u7528\u6237\u8F93\u5165\u3001\u66F4\u9AD8\u6548\
  \u5730\u89E3\u6790\u6587\u672C\u6570\u636E\u6216\u4E3A\u8FDB\u4E00\u6B65\u5904\u7406\
  \u9700\u8981\u65E0\u5F15\u53F7\u5185\u5BB9\u7684\u5B57\u7B26\u4E32\u505A\u51C6\u5907\
  \u3002"
lastmod: 2024-02-18 23:08:58.696181
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7\u662F\
  \u6307\u53BB\u9664\u7ED9\u5B9A\u5B57\u7B26\u4E32\u9996\u5C3E\u7684\u5F15\u53F7(`\"\
  ` \u6216 `'`)\u3002\u7A0B\u5E8F\u5458\u5E38\u5E38\u9700\u8981\u6267\u884C\u8FD9\u4E00\
  \u4EFB\u52A1\u4EE5\u6E05\u6D17\u7528\u6237\u8F93\u5165\u3001\u66F4\u9AD8\u6548\u5730\
  \u89E3\u6790\u6587\u672C\u6570\u636E\u6216\u4E3A\u8FDB\u4E00\u6B65\u5904\u7406\u9700\
  \u8981\u65E0\u5F15\u53F7\u5185\u5BB9\u7684\u5B57\u7B26\u4E32\u505A\u51C6\u5907\u3002"
title: "\u5220\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\u5F15\u53F7"
---

{{< edit_this_page >}}

## 什么和为什么？

在 Go 中从字符串中移除引号是指去除给定字符串首尾的引号(`"` 或 `'`)。程序员常常需要执行这一任务以清洗用户输入、更高效地解析文本数据或为进一步处理需要无引号内容的字符串做准备。

## 如何做：

Go 提供了几种从字符串中移除引号的方法，但最直接的方法之一是使用 `strings` 包提供的 `Trim` 和 `TrimFunc` 函数。以下是如何做到的：

```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	quotedString := `"This is a 'quoted' string"`

	// 使用 strings.Trim 移除特定引号
	unquoted := strings.Trim(quotedString, `"'`)
	fmt.Println("使用 strings.Trim:", unquoted)

	// 使用 strings.TrimFunc 的自定义方式获得更多控制
	unquotedFunc := strings.TrimFunc(quotedString, func(r rune) bool {
		return r == '"' || r == '\''
	})
	fmt.Println("使用 strings.TrimFunc:", unquotedFunc)
}
```

这个例子演示了移除双引号(`"`)和单引号(`'`)的两种方法。`strings.Trim` 函数更简单，当你确切知道要移除哪些字符时，它非常有效。另一方面，`strings.TrimFunc` 提供了更多的灵活性，允许你指定一个自定义的函数来决定哪些字符被移除。上述代码的示例输出为：

```
使用 strings.Trim: This is a 'quoted' string
使用 strings.TrimFunc: This is a 'quoted' string
```

这两种方法都有效地从字符串中移除了首尾的引号。

## 深入了解

`strings` 包中的 `Trim` 和 `TrimFunc` 函数是 Go 广泛标准库的一部分，旨在提供强大而简单的字符串处理能力，而无需第三方包。从历史上看，高效处理和操作字符串的需求源于 Go 主要关注网络服务器和数据解析器，其中字符串处理是一项常见任务。

这些函数基于 runes（Go 对 Unicode 码点的表示）的实现是其一个显著特点。这种设计使它们能够无缝处理包含多字节字符的字符串，使 Go 的字符串操作方法既健壮又友好地支持 Unicode。

虽然直接使用 `Trim` 和 `TrimFunc` 移除引号在 Go 中是方便且惯用的，但值得一提的是，对于更复杂的字符串处理任务（例如，嵌套引号、转义引号），正则表达式（通过 `regexp` 包）或手动解析可能提供更好的解决方案。然而，这些替代方法带来了增加的复杂性和性能考虑。因此，对于简单的引号移除，所演示的方法在简单性、性能和功能性之间取得了良好的平衡。
