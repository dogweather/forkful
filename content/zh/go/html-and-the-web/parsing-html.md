---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:00:09.357890-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728Go\u4E2D\u89E3\u6790HTML\uFF0C\
  \u4F60\u901A\u5E38\u4F1A\u4F7F\u7528`goquery`\u5305\u6216\u6807\u51C6\u5E93\u7684\
  `net/html`\u5305\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528`net/html`\u7684\
  \u57FA\u7840\u793A\u4F8B\uFF0C\u5C55\u793A\u4E86\u5982\u4F55\u4ECE\u7F51\u9875\u4E2D\
  \u63D0\u53D6\u6240\u6709\u94FE\u63A5\uFF1A."
lastmod: '2024-04-05T22:38:46.327282-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728Go\u4E2D\u89E3\u6790HTML\uFF0C\
  \u4F60\u901A\u5E38\u4F1A\u4F7F\u7528`goquery`\u5305\u6216\u6807\u51C6\u5E93\u7684\
  `net/html`\u5305\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528`net/html`\u7684\
  \u57FA\u7840\u793A\u4F8B\uFF0C\u5C55\u793A\u4E86\u5982\u4F55\u4ECE\u7F51\u9875\u4E2D\
  \u63D0\u53D6\u6240\u6709\u94FE\u63A5\uFF1A."
title: "\u89E3\u6790HTML"
weight: 43
---

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
