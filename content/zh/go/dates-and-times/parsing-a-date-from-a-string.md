---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:01.472344-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Go \u901A\u8FC7 `time` \u5305\u4E3A\u89E3\
  \u6790\u65E5\u671F\u548C\u65F6\u95F4\u63D0\u4F9B\u4E86\u5F3A\u5927\u7684\u652F\u6301\
  \u3002\u5173\u952E\u662F\u8981\u7406\u89E3 Go \u7684\u53C2\u8003\u65E5\u671F\u683C\
  \u5F0F\uFF1A`Mon Jan 2 15:04:05 MST 2006`\uFF0C\u4F60\u4F7F\u7528\u5B83\u6765\u544A\
  \u8BC9 Go \u5982\u4F55\u89E3\u91CA\u4F20\u5165\u7684\u5B57\u7B26\u4E32\u3002\u8FD9\
  \u91CC\u6709\u4E00\u4E2A\u5FEB\u901F\u793A\u4F8B\uFF0C\u5E2E\u52A9\u4F60\u5F00\u59CB\
  \uFF1A."
lastmod: '2024-04-05T22:38:46.342177-06:00'
model: gpt-4-0125-preview
summary: "04:05 MST 2006`\uFF0C\u4F60\u4F7F\u7528\u5B83\u6765\u544A\u8BC9 Go \u5982\
  \u4F55\u89E3\u91CA\u4F20\u5165\u7684\u5B57\u7B26\u4E32\u3002\u8FD9\u91CC\u6709\u4E00\
  \u4E2A\u5FEB\u901F\u793A\u4F8B\uFF0C\u5E2E\u52A9\u4F60\u5F00\u59CB\uFF1A."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 如何操作：
Go 通过 `time` 包为解析日期和时间提供了强大的支持。关键是要理解 Go 的参考日期格式：`Mon Jan 2 15:04:05 MST 2006`，你使用它来告诉 Go 如何解释传入的字符串。这里有一个快速示例，帮助你开始：

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 示例日期字符串
	dateStr := "2023-04-12 14:45:00"
	
	// 定义输入日期字符串的布局/格式
	// 这个布局告诉 Go 期望一个年份，接着是月份，
	// 然后是日期、小时、分钟，最后是秒
	layout := "2006-01-02 15:04:05"
	
	// 根据布局解析日期字符串
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("解析日期出错：", err)
		return
	}
	
	// 输出解析后的日期
	fmt.Println("解析后的日期：", parsedDate)
}
```

当你运行这段代码时，你会得到：

```
解析后的日期：2023-04-12 14:45:00 +0000 UTC
```

注意 `layout` 字符串是如何使用参考日期的值来指定输入字符串的格式。根据你的输入日期的格式调整 `layout`。

## 深入了解
Go 的日期和时间解析设计独特，利用了一个特定的参考日期（`Mon Jan 2 15:04:05 MST 2006`）。这种方法，与使用更常规的格式说明符（如 `YYYY` 代表年份）不同，是为了可读性和易用性而选择的，采用了更基于示例的格式。

虽然这最初可能对习惯于其他语言的程序员来说看起来不寻常，但许多人在短暂的调整期后会发现它更直观。对于需要更复杂的日期操作或 Go 的 `time` 包直接不支持的格式的应用，第三方库例如 `github.com/jinzhu/now` 可以提供额外的功能。然而，对于大多数标准应用来说，Go 的内置能力是强大的、高性能的，并且惯用的，体现了 Go 的简洁和清晰的理念。
