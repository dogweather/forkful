---
title:                "Ruby: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## 为什么

Mandarin: 所以，你想学习如何解析HTML？那么让我告诉你为什么这是一个很好的决定！

如果你想要从网页中提取特定的信息，例如产品价格或者文章标题，那么解析HTML就是必不可少的技能。HTML是构建网页的标记语言，它是浏览器显示网页内容的基础。通过解析HTML，你可以轻松地从网页源代码中提取你需要的信息，从而加快数据收集和处理的过程。

## 如何做

Mandarin: 现在让我们来看看如何使用Ruby来解析HTML吧！我会给出一些简单的例子并展示输出结果。记得使用```Ruby ... ```代码块来编写你的Ruby代码哦。

例子1：解析网页标题

```Ruby
require 'nokogiri'
require 'open-uri'

# 打开网页
html = open("https://www.example.com")

# 使用Nokogiri来解析HTML
doc = Nokogiri::HTML(html)

# 提取页面标题
title = doc.css('title').text

# 打印输出
puts title
```

输出结果：Example Domain

例子2：提取所有超链接

```Ruby
require 'nokogiri'
require 'open-uri'

# 打开网页
html = open("https://www.example.com")

# 使用Nokogiri来解析HTML
doc = Nokogiri::HTML(html)

# 选择所有超链接
links = doc.css('a')

# 遍历并打印输出每一个超链接
links.each do |link|
    puts link['href']
end
```

输出结果：

https://www.iana.org/domains/example
https://www.iana.org/domains/example

## 深入了解

Mandarin: 如果你想更深入地了解HTML的解析过程，可以看一下Nokogiri和其他类似的库的文档。它们都提供了强大的CSS选择器来帮助你提取数据，并支持XPath和正则表达式等其他解析方法。此外，你也可以学习一些HTML和CSS的基础知识，这将有助于你更好地理解网页结构和选择器的使用。

## 更多资料

Mandarin: 如果你想继续学习HTML解析，可以参考以下链接：

- Nokogiri文档：https://nokogiri.org/
- Ruby官方文档：https://www.ruby-lang.org/en/documentation/
- HTML教程：https://www.w3schools.com/html/
- CSS教程：https://www.w3schools.com/css/

## 参见

Mandarin: 这篇文章主要讨论了解析HTML的基础知识和方法，但HTML解析还有许多其他可以探索的方面。如果你对此感兴趣，可以参考以下链接来继续学习：

- 网页抓取技术：https://www.geeksforgeeks.org/web-scraping-introduction-and-advantages/
- 前端开发：https://www.freecodecamp.org/news/what-is-front-end-development/
- 数据抓取工具：https://www.scrapingbee.com/blog/web-scraping-tools/