---
title:                "解析HTML"
html_title:           "Ruby: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## 为什么

为什么要学习使用 Ruby 解析 HTML？因为作为一门强大的编程语言，Ruby 提供了丰富的工具来轻松地解析和处理 HTML 文件，让我们可以更加灵活地提取所需的信息。

## 如何实现

通过使用 Ruby 的一个强大的库 Nokogiri，我们可以轻松实现对 HTML 文件的解析和处理。下面是一个简单的例子来演示如何使用 Nokogiri 来解析一个包含链接的 HTML 文件，并输出链接的标题和 URL。

```Ruby
require 'nokogiri'

# 将 HTML 文件加载进 Nokogiri 的文档对象
doc = Nokogiri::HTML(File.read('example.html'))

# 使用 CSS 选择器来定位所有链接元素
links = doc.css('a')

# 遍历每个链接并输出标题和 URL
links.each do |link|
  puts "标题：#{link.text}"
  puts "URL：#{link['href']}"
  puts "-----------------------"
end
```

输出：

```
标题：Ruby官方网站
URL：https://www.ruby-lang.org/
-----------------------
标题：Ruby中国用户论坛
URL：https://ruby-china.org/
-----------------------
标题：Ruby Toolbox
URL：https://www.ruby-toolbox.com/
-----------------------
```

## 深入了解

HTML 是一种标记语言，用来描述网页的结构和内容。它通常由各种标签组成，标签之间可以包含属性和文本。Nokogiri 这样的库可以帮助我们解析这些标签，提供方便的接口来获取和处理 HTML 文件中的内容。

除了 CSS 选择器之外，Nokogiri 还提供了强大的 XPath 查询语言来定位 HTML 元素。它也支持修改 HTML 文件，并能够快速解析大型文件。更多关于 Nokogiri 的用法和功能，请查阅官方文档。

## 参考链接

- [Nokogiri 官方文档](https://www.rubydoc.info/github/sparklemotion/nokogiri)
- [CSS 选择器教程](https://www.w3schools.com/cssref/css_selectors.asp)
- [XPath 查询语言指南](https://www.w3schools.com/xml/xpath_intro.asp)