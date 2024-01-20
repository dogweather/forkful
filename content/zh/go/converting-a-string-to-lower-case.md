---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么和为什么？
字符串变小写，指的是将编程中的文本字符串转换为全部小写字母。我们程序员之所以这么做，一是为了统一数据格式，二是利于字符比较。

## 如何操作：
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	var str string = "WELCOME TO THE WORLD OF GOLANG"
	fmt.Println(strings.ToLower(str))
}	
```
运行上述代码，会得到全部小写的字符串 "welcome to the world of golang"。

## 深入探索：
1. 历史背景：从早期的编程语言（如C）开始，至今许多语言都内置了字符串转小写的函数。
2. 选择方案：不同编程语言有不同的实现方式，例如PHP有`strtolower()`函数，Python有`lower()`方法.
3. 实现细节：在Go语言中，`ToLower()`函数是通过遍历并替换字符串中的每个大写字母实现的。

## 参考链接：
1. Go语言标准库文档：https://golang.org/pkg/strings/#ToLower
2. Go语言中级教程：https://medium.com/@IndianGuru
3. “转为小写”在各种编程语言中的实现：https://rosettacode.org/wiki/Case_conversion