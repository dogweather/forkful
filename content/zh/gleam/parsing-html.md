---
title:                "解析 HTML"
html_title:           "Gleam: 解析 HTML"
simple_title:         "解析 HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## 为什么

在编写网页或者进行网络爬虫时，经常需要从大量的HTML代码中提取所需信息。使用Gleam来解析HTML可以让这一过程更加简单、高效，节省大量的时间和精力。

## 如何操作

首先，我们需要导入Gleam的HTML解析库`Html.Parser`。然后，使用`Html.Parser.parse_string()`方法来解析HTML代码。例如，下面是一个简单的示例，从一个HTML代码片段中提取所有的“<h1>”标签的内容，并打印出来。

```Gleam
import Html.Parser

let html = """
<html>
  <body>
    <h1>Hello World</h1>
    <h1>Gleam is Awesome</h1>
  </body>
</html>
"""

let parsed = Html.Parser.parse_string(html)

// Print all "h1" tags' content
for node in parsed.headings {
    println(node.inner_text)
}

// Output:
// Hello World
// Gleam is Awesome
```

## 深入探讨

Gleam的HTML解析功能可以更加灵活和强大。它可以提取不同类型的HTML标签，如段落标签、链接标签等，并且可以根据需要提取标签的属性值。我们也可以使用`Html.Parser.parse_url()`方法来从指定的URL地址中获取HTML代码，并进行解析。详细的使用说明和方法可参考Gleam的官方文档。

## 参考链接

- [Gleam官方网站](https://gleam.run/)
- [Gleam HTML解析库文档](https://gleam.run/modules/html.html)
- [Gleam HTML解析库源代码](https://github.com/gleam-lang/html)