---
title:                "解析HTML"
html_title:           "Gleam: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/parsing-html.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？

HTML 解析是指从网站或应用程序中提取和解析 HTML 代码的过程。程序员通常会进行 HTML 解析，以便从网页上获取所需的数据，或者对网页的内容进行修改和操作。

# 怎样做？

您可以使用 Gleam 编程语言来轻松地解析 HTML 代码。下面是一个示例，展示了如何使用 Gleam 中的“Html-Parser”包来解析 HTML 代码：

```Gleam
use Html.Parser

let html = "
<html>
    <head>
        <title>Gleam Programming Article</title>
    </head>
    <body>
        <h1>Hello, world!</h1>
        <p>This is a paragraph.</p>
    </body>
</html>
"

let parsed_html = Html.Parser.parse(html)

// Output: 
// <Gleam Html.Node>
// <tag_name: "html">
// <attributes: []>
// <children: [
//     <Gleam Html.Node>
//     <tag_name: "head">
//     <attributes: []>
//     <children: [
//         <Gleam Html.Node>
//         <tag_name: "title">
//         <attributes: []>
//         <children: [
//             <Gleam Html.Text>: "Gleam Programming Article"
//         ]>
//     ]>,
//     <Gleam Html.Node>
//     <tag_name: "body">
//     <attributes: []>
//     <children: [
//         <Gleam Html.Node>
//         <tag_name: "h1">
//         <attributes: []>
//         <children: [
//             <Gleam Html.Text>: "Hello, world!"
//         ]>,
//         <Gleam Html.Node>
//         <tag_name: "p">
//         <attributes: []>
//         <children: [
//             <Gleam Html.Text>: "This is a paragraph."
//         ]>
//     ]>
// ]>

```

# 深入了解

HTML 解析是互联网发展史上的一个重要组成部分。在早期的互联网时代，网页的内容仅由 HTML 代码组成，因此解析 HTML 代码就是获取网页内容的唯一方法。随着互联网的发展，出现了其他解析技术，如 DOM 解析和 CSS 选择器，但 HTML 解析仍然是常用的数据提取方式之一。

除了 Gleam 中的“Html-Parser”包外，还有其他工具可以帮助您解析 HTML 代码，如 Beautiful Soup 和 GoQuery。

HTML 解析的实现原理主要是通过解析器（parser）读取 HTML 代码的各个标签和内容，并将它们转换为节点（node）对象，以便程序可以对其进行处理。

# 链接

- Gleam编程语言官方网站：https://gleam.run/
- Gleam中的HTML解析器库：https://github.com/gleam-lang/html-parser
- Beautiful Soup使用指南：https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- GoQuery使用指南：https://github.com/PuerkitoBio/goquery