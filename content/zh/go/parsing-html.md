---
title:                "解析HTML"
date:                  2024-01-20T15:31:46.681732-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? 解析HTML是什么？为什么程序员要做这件事？
解析HTML意味着将HTML文本转换成可由程序理解和操作的结构。程序员这样做是为了提取数据、自动化测试网页或进行网站内容管理。

## How to: 怎么做
Go语言解析HTML很直接。用`"golang.org/x/net/html"`包可以轻松完成。下面是个简单的例子：

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"strings"
)

func main() {
	// 示例HTML字符串
	rawHTML := `<html><body><p>Hello, World!</p></body></html>`

	// 将字符串解析为HTML节点
	doc, err := html.Parse(strings.NewReader(rawHTML))
	if err != nil {
		panic("无法解析HTML: " + err.Error())
	}

	// 递归遍历HTML节点树，并打印节点类型
	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode {
			fmt.Println(n.Data)
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)
}

```

运行上面的代码，会输出：

```
html
body
p
```

## Deep Dive 深入探究
解析HTML可以追溯到网页诞生的时候。当初，解析HTML主要是浏览器的工作。但后来随着网络的开发，后端服务也开始需要解析HTML，例如搜索引擎和各种自动化工具。

除了`"golang.org/x/net/html"`包，还可以使用第三方库，比如`goquery`，它提供了更加丰富的功能和jQuery风格的操作方式。

解析HTML时要注意HTML的特性，比如松散的语法和多样的编码方式。解析算法通常需要能应对不规则的HTML并优雅地恢复错误。

## See Also 参考链接
- Go语言官方文档: https://pkg.go.dev/golang.org/x/net/html
- goquery库: https://pkg.go.dev/github.com/PuerkitoBio/goquery
- 维基百科上的HTML解析: https://zh.wikipedia.org/wiki/HTML%E8%A7%A3%E6%9E%90器
