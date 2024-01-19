---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/parsing-html.md"
---

{{< edit_this_page >}}

## 什么&为什么？

解析HTML就是将HTML文本转换为计算机能理解的数据结构的过程。程序员之所以要做这个，是因为他们需要从网页中提取需要的数据，或者对HTML进行更复杂的操作。

## 如何做：

```Go 
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"os"
)

func main() {
	doc, err := html.Parse(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stderr, "findlinks1: %v\n", err)
		os.Exit(1)
	}
	for _, link := range visit(nil, doc) {
		fmt.Println(link)
	}
}

func visit(links []string, n *html.Node) []string {
	if n.Type == html.ElementNode && n.Data == "a" {
		for _, a := range n.Attr {
			if a.Key == "href" {
				links = append(links, a.Val)
			}
		}
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		links = visit(links, c)
	}
	return links
}
```

当您运行这个示例代码时，将到标准输入(stdin)输入一些HTML，它会输出所有的链接。

## 深入探究

历史背景：HTML解析是从web早期开始的，当时网页结构比较简单，很少有JavaScript和CSS。

解析HTML的替代方法：除去Go，还有很多不同的编程语言和库可以进行HTML解析，比如Python的BeautifulSoup，Java的Jsoup等等。

关于解析HTML的实现细节：Go的`html`库用一种称为“标记树”(Token Tree)的数据结构来存储HTML，这个数据结构适合表示HTML文档的嵌套结构。

## 更多信息

关于更多的Go编程技巧和HTML解析的资料，欢迎访问以下网站：

1. "[Go Programming Language](https://golang.org/)" - Go语言的官方网站，你可以在这里找到详细的Go编程指南和教程。

2. "[HTML Parsing in Go](https://dzone.com/articles/html-parsing-go)" - 这篇文章详细介绍了在Go中解析HTML的基础知识。