---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:43.520856-07:00
description: "\u5728 Go \u8BED\u8A00\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\
  \u7A0B\u5E8F\u5458\u7684\u57FA\u672C\u4EFB\u52A1\uFF0C\u7C7B\u4F3C\u4E8E\u5176\u666E\
  \u904D\u6027\u7684 \"Hello, World!\"\u3002\u5B83\u5BF9\u4E8E\u4ECE\u8BB0\u5F55\u548C\
  \u65F6\u95F4\u6233\u4E8B\u4EF6\u5230\u8BA1\u7B97\u6301\u7EED\u65F6\u95F4\u548C\u5B89\
  \u6392\u672A\u6765\u4E8B\u4EF6\u7684\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\u3002"
lastmod: 2024-02-19 22:05:06.232633
model: gpt-4-0125-preview
summary: "\u5728 Go \u8BED\u8A00\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u7A0B\
  \u5E8F\u5458\u7684\u57FA\u672C\u4EFB\u52A1\uFF0C\u7C7B\u4F3C\u4E8E\u5176\u666E\u904D\
  \u6027\u7684 \"Hello, World!\"\u3002\u5B83\u5BF9\u4E8E\u4ECE\u8BB0\u5F55\u548C\u65F6\
  \u95F4\u6233\u4E8B\u4EF6\u5230\u8BA1\u7B97\u6301\u7EED\u65F6\u95F4\u548C\u5B89\u6392\
  \u672A\u6765\u4E8B\u4EF6\u7684\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\u3002"
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么及为什么？

在 Go 语言中获取当前日期是程序员的基本任务，类似于其普遍性的 "Hello, World!"。它对于从记录和时间戳事件到计算持续时间和安排未来事件的任务至关重要。

## 如何操作：

在 Go 中，`time` 包是你处理日期和时间的入口。`time.Now()` 函数给你当前的日期和时间，而其他函数和方法允许你格式化或操作这些数据。以下是如何获取当前日期及其各种表现形式：

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // 获取当前的日期和时间
	fmt.Println("当前时间：", currentTime)

	// 获取 YYYY-MM-DD 格式的日期
	fmt.Println("当前日期：", currentTime.Format("2006-01-02"))

	// 获取日期的单独组成部分
	year, month, day := currentTime.Date()
	fmt.Printf("年：%d, 月：%s, 日：%d\n", year, month, day)

	// 获取星期几
	fmt.Println("星期：", currentTime.Weekday())
}
```

示例输出可能看起来像这样：

```
当前时间：2023-04-18 15:04:05.123456 +0000 UTC
当前日期：2023-04-18
年：2023, 月：April, 日：18
星期：Tuesday
```

注意 `Format` 是如何使用一个特定日期（2006-01-02）作为布局字符串的。这是 Go 选择的参考日期，作为格式化日期的记忆模式。

## 深入探讨

在 Go 中使用 `time` 包来处理日期和时间的决定反映了该语言对强大且直观的标准库的承诺。与可能有多个竞争库或日期操作方法的某些语言不同，Go 优先考虑拥有一个单一、文档齐全的标准。

Go 时间格式化中参考日期（`Mon Jan 2 15:04:05 MST 2006`）的独特选择，虽然一开始可能令人困惑，实际上是在可用性方面的一个高超之举。它允许程序员使用基于示例的方法来表示日期和时间格式，而不是像其他语言可能使用的记忆标记或符号。

话虽如此，虽然 `time` 包为大多数需求提供了全面的功能，但处理时区和夏令时（DST）变化有时可能会让新的 Go 程序员困惑。理解 Go 如何处理特定位置的时间以避免时间操作中的常见陷阱至关重要。

对于更复杂的调度或时间操作需求，第三方库如 Go 的 `github.com/robfig/cron` 可能比标准的 `time` 包提供更专业的功能。然而，对于需要获取和处理当前日期及时间的大多数应用来说，`time` 包提供了 Go 中一个坚实且符合惯用法的起点。
