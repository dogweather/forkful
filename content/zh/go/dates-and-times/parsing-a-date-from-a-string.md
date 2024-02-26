---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:01.472344-07:00
description: "\u5728 Go \u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\
  \uFF0C\u6D89\u53CA\u5C06\u4EE5\u6587\u672C\u5F62\u5F0F\u8868\u793A\u7684\u65E5\u671F\
  \u8F6C\u6362\u4E3A\u66F4\u53EF\u7528\u7684\u683C\u5F0F\uFF08\u4F8B\u5982\uFF0C`time.Time`\uFF09\
  \u3002\u7A0B\u5E8F\u5458\u6267\u884C\u6B64\u4EFB\u52A1\u662F\u4E3A\u4E86\u5728\u5E94\
  \u7528\u4E2D\u66F4\u51C6\u786E\u5730\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\u6570\
  \u636E\uFF0C\u7279\u522B\u662F\u5728\u5904\u7406\u7528\u6237\u8F93\u5165\u3001API\
  \ \u6216\u5B58\u50A8\u7CFB\u7EDF\u65F6\uFF0C\u8FD9\u4E9B\u573A\u5408\u7684\u65E5\
  \u671F\u901A\u5E38\u4EE5\u5B57\u7B26\u4E32\u5F62\u5F0F\u8868\u793A\u3002"
lastmod: '2024-02-25T18:49:44.787467-07:00'
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\uFF0C\
  \u6D89\u53CA\u5C06\u4EE5\u6587\u672C\u5F62\u5F0F\u8868\u793A\u7684\u65E5\u671F\u8F6C\
  \u6362\u4E3A\u66F4\u53EF\u7528\u7684\u683C\u5F0F\uFF08\u4F8B\u5982\uFF0C`time.Time`\uFF09\
  \u3002\u7A0B\u5E8F\u5458\u6267\u884C\u6B64\u4EFB\u52A1\u662F\u4E3A\u4E86\u5728\u5E94\
  \u7528\u4E2D\u66F4\u51C6\u786E\u5730\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\u6570\
  \u636E\uFF0C\u7279\u522B\u662F\u5728\u5904\u7406\u7528\u6237\u8F93\u5165\u3001API\
  \ \u6216\u5B58\u50A8\u7CFB\u7EDF\u65F6\uFF0C\u8FD9\u4E9B\u573A\u5408\u7684\u65E5\
  \u671F\u901A\u5E38\u4EE5\u5B57\u7B26\u4E32\u5F62\u5F0F\u8868\u793A\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Go 中解析字符串中的日期，涉及将以文本形式表示的日期转换为更可用的格式（例如，`time.Time`）。程序员执行此任务是为了在应用中更准确地处理日期和时间数据，特别是在处理用户输入、API 或存储系统时，这些场合的日期通常以字符串形式表示。

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
