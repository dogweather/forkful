---
title:                "从字符串解析日期"
date:                  2024-01-20T15:36:28.346040-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么与为什么？)
从字符串解析日期是指把文本形式的日期转换为计算机能理解的格式。程序员这么做是为了处理和存储日期数据，方便日期比较、计算和转换。

## How to: (如何操作：)
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 日期字符串
	dateStr := "2023-04-12"
	
	// 定义布局
	layout := "2006-01-02"
	
	// 解析日期
	date, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Error parsing date:", err)
		return
	}
	
	// 打印结果
	fmt.Println("Parsed Date:", date)
}
```
输出:
```
Parsed Date: 2023-04-12 00:00:00 +0000 UTC
```

## Deep Dive (深入探讨)
Go语言自带的`time`包提供了日期解析功能。`time.Parse`函数接受两个字符串参数，一个是布局(layout)，另一个是待解析的日期字符串。布局是用来告诉Go如何理解日期结构的，以 `2006-01-02 15:04:05` 这个特定日期(固定选择这日期作为模板)来指定各个部分。

替代方案有使用第三方库如`dateparse`或正则表达式，但`time`包的简洁性和强大通常是首选。在内部，解析函数利用布局字符串中的日期和时间点对应数值来理解输入字符串，并构建一个`time.Time`对象。

## See Also (另请参阅)
- Go官方文档中的`time`包: [time package](https://pkg.go.dev/time)
- 关于`time.Parse`函数的更多信息: [time.Parse function](https://pkg.go.dev/time#Parse)
- 一个流行的Go语言日期解析第三方库: [dateparse](https://github.com/araddon/dateparse)