---
title:                "解析HTML"
html_title:           "C#: 解析HTML"
simple_title:         "解析HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## 为什么

网页上的信息都是以HTML代码的形式存在，而HTML代码通常是冗长且难以阅读的。因此，解析HTML成为了获取有用信息的必要步骤。

## 如何使用

```C#
// 导入HtmlAgilityPack库
using HtmlAgilityPack;

// 创建HtmlDocument对象
HtmlDocument doc = new HtmlDocument();

// 从URL获取HTML代码
doc.Load("https://www.example.com/");

// 选择某个HTML元素进行解析
HtmlNode node = doc.DocumentNode.SelectSingleNode("//div[@class='title']");

// 获取元素的文本内容
string title = node.InnerText;

// 输出结果
Console.WriteLine("标题：{0}", title);
```

文本输出：标题：这是网页的标题

## 深入探讨

解析HTML并不仅仅是简单地获取某个元素的文本内容。使用HtmlAgilityPack库，可以实现更复杂的操作，例如：选择多个HTML元素、从特定位置开始选择元素、根据CSS类名进行选择、提取属性值等等。通过深入学习和使用该库，可以更加灵活地提取出想要的信息。

## 参考链接

- [HtmlAgilityPack](https://html-agility-pack.net/)
- [C#官方文档](https://docs.microsoft.com/en-us/dotnet/csharp/)
- [C#入门教程](https://code.visualstudio.com/docs/languages/csharp)
- [W3School HTML教程](https://www.w3school.com.cn/html/)
- [W3School XPath教程](https://www.w3school.com.cn/xpath/)