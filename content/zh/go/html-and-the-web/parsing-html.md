---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:00:09.357890-07:00
description: "\u5728Go\u8BED\u8A00\u4E2D\u89E3\u6790HTML\u6D89\u53CA\u5230\u5206\u6790\
  HTML\u6587\u4EF6\u7684\u5185\u5BB9\uFF0C\u4EE5\u63D0\u53D6\u6570\u636E\u3001\u64CD\
  \u4F5C\u7ED3\u6784\u6216\u5C06HTML\u8F6C\u6362\u4E3A\u5176\u4ED6\u683C\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u4E3A\u4E86\u8FDB\
  \u884C\u7F51\u9875\u6293\u53D6\u3001\u6A21\u677F\u751F\u6210\u548C\u6570\u636E\u6316\
  \u6398\uFF0C\u5229\u7528Go\u5F3A\u5927\u7684\u5E76\u53D1\u529F\u80FD\uFF0C\u9AD8\
  \u6548\u5904\u7406\u5927\u91CF\u7F51\u9875\u6570\u636E\u3002"
lastmod: 2024-02-19 22:05:06.215053
model: gpt-4-0125-preview
summary: "\u5728Go\u8BED\u8A00\u4E2D\u89E3\u6790HTML\u6D89\u53CA\u5230\u5206\u6790\
  HTML\u6587\u4EF6\u7684\u5185\u5BB9\uFF0C\u4EE5\u63D0\u53D6\u6570\u636E\u3001\u64CD\
  \u4F5C\u7ED3\u6784\u6216\u5C06HTML\u8F6C\u6362\u4E3A\u5176\u4ED6\u683C\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u4E3A\u4E86\u8FDB\
  \u884C\u7F51\u9875\u6293\u53D6\u3001\u6A21\u677F\u751F\u6210\u548C\u6570\u636E\u6316\
  \u6398\uFF0C\u5229\u7528Go\u5F3A\u5927\u7684\u5E76\u53D1\u529F\u80FD\uFF0C\u9AD8\
  \u6548\u5904\u7406\u5927\u91CF\u7F51\u9875\u6570\u636E\u3002"
title: "\u89E3\u6790HTML"
---

{{< edit_this_page >}}

## 什么与为什么？

在Go语言中解析HTML涉及到分析HTML文件的内容，以提取数据、操作结构或将HTML转换为其他格式。程序员之所以这样做，是为了进行网页抓取、模板生成和数据挖掘，利用Go强大的并发功能，高效处理大量网页数据。

## 如何操作：

要在Go中解析HTML，你通常会使用`goquery`包或标准库的`net/html`包。这里有一个使用`net/html`的基础示例，展示了如何从网页中提取所有链接：

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // 获取HTML文档
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // 解析HTML文档
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // 递归遍历DOM的函数
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // 遍历DOM
    f(doc)
}
```

假设`http://example.com`包含两个链接的样例输出：

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

该代码请求一个HTML页面，解析它，并递归遍历DOM，找到并打印所有`<a>`标签的`href`属性。

## 深入了解

`net/html`包为Go语言中的HTML解析提供了基础，直接实现了由HTML5标准指定的标记化和树构建算法。这种低级方法虽然强大，但对于复杂任务来说可能过于冗长。

相比之下，第三方`goquery`包受到jQuery的启发，提供了一个更高级别的接口，简化了DOM操作和遍历。它使开发人员能够为元素选择、属性提取和内容操作等任务编写简洁、富有表达力的代码。

然而，`goquery`的便利性以一个额外的依赖和由于其抽象层可能导致的性能降低为代价。在`net/html`和`goquery`（或其他解析库）之间的选择取决于项目的具体需求，比如对性能优化或易用性的需求。

历史上，Go中的HTML解析已经从基本的字符串操作演变为复杂的DOM树操作，反映了该语言不断增长的生态系统和社区对健壮的网页抓取和数据提取工具的需求。尽管有内置能力，但像`goquery`这样的第三方库的盛行，凸显了Go社区对模块化、可重用代码的偏好。然而，对于性能关键的应用程序，程序员可能仍然更喜欢`net/html`包，甚至在简单解析任务中使用正则表达式，同时意识到基于正则的HTML解析固有的风险和限制。
