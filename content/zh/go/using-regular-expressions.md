---
title:                "使用正则表达式"
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么用？)
正则表达式是用来匹配字符串模式的。程序员用它来搜索、替换、验证文本数据。

## How to (如何使用)
```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// 匹配email地址
	re := regexp.MustCompile(`\b[\w\.-]+@[\w\.-]+\.\w{2,4}\b`)
	fmt.Println(re.MatchString("test@email.com")) // 输出: true
	fmt.Println(re.MatchString("no-at-symbol"))   // 输出: false
	
	// 查找匹配项
	result := re.FindString("contact: test@email.com")
	fmt.Println(result)  // 输出: "test@email.com"
	
	// 替换文本
	replaced := re.ReplaceAllString("reach me at test@email.com", "hidden")
	fmt.Println(replaced) // 输出: "reach me at hidden"
}
```

## Deep Dive (深入探究)
1. **历史背景**: 正则表达式起源于20世纪60年代的理论计算机科学。
2. **替代品**: 除了正则表达式，你也可以使用字符串匹配和解析库。
3. **实现细节**: Go的regexp包是基于RE2引擎，相比其他引擎避免了某些复杂正则表达式的安全问题。

## See Also (了解更多)
- Go语言正则表达式官方文档: https://pkg.go.dev/regexp
- 正则表达式的入门教程: http://www.regular-expressions.info/tutorial.html
- 玩转正则表达式的更多示例: https://github.com/google/re2/wiki/Syntax
