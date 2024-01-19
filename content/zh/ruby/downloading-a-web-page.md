---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么和为什么？
下载网页就是将互联网上的某个网页内容获取并保存到本地的过程。编程人员这么做主要是为了分析网页内容，或者将网页内容用于离线阅读。

## 如何操作：
在Ruby中，我们可以用 `open-uri` 和 `nokogiri` 这两个库来下载并分析网页。以下是一个简单的例子：

```Ruby
require 'open-uri'
require 'nokogiri'

page = Nokogiri::HTML(open('https://www.example.com'))
puts page.title
```
如果你运行这段代码，你将看到输出为 "Example Domain"。这就是你从 https://www.example.com 这个网页获取的标题。

## 深入探讨
在早期，在对网页内容进行下载和解析的过程中，我们常常需要用到很复杂的方法。而有了 `open-uri` 和 `nokogiri` 等库，这个过程变得相当简单。对于其他语言，例如Python，你可能需要使用 `requests` 和 `beautifulsoup4` 等库。

在实现下载网页的功能时，一种可能的做法是直接将网页内容下载到内存中，然后再进行分析。然而这种做法会消耗大量的内存资源，尤其是当前下载的是大型网页的时候。因此，一种更好的做法是使用流的方式来下载和解析网页。

## 另请参见
- Nokogiri 教程：http://www.nokogiri.org/tutorials/
- Ruby open-uri 官方文档：https://ruby-doc.org/stdlib-2.6.1/libdoc/open-uri/rdoc/OpenURI.html