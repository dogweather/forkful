---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:04.997869-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728Ruby\u4E2D\u89E3\u6790HTML\uFF0C\
  \u8BF7\u7528`gem install nokogiri`\u5B89\u88C5'Nokogiri'\u5B9D\u77F3\u3002Nokogiri\u5C31\
  \u50CF\u662F\u7528\u4E8E\u5728Ruby\u4E2D\u5904\u7406HTML\u548CXML\u7684\u745E\u58EB\
  \u519B\u5200\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u5FEB\u901F\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T21:53:48.647656-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何操作：
要在Ruby中解析HTML，请用`gem install nokogiri`安装'Nokogiri'宝石。Nokogiri就像是用于在Ruby中处理HTML和XML的瑞士军刀。这里有一个快速示例：

```ruby
require 'nokogiri'
require 'open-uri'

# 从网站加载HTML内容
html_content = URI.open('http://example.com').read

# 解析HTML
doc = Nokogiri::HTML(html_content)

# 提取标题
title = doc.xpath('//title').text
puts "该页面的标题是：#{title}"
```

这会输出类似于：`该页面的标题是：Example Domain`的东西。

## 深入探讨
回到早期的Ruby日子，解析HTML的选项有限。REXML内置但缓慢。然后出现了Hpricot，但它逐渐淡出。Nokogiri在2008年面世，将Hpricot的易用性与libxml的速度和力量结合起来，libxml是一个经过验证的XML工具包。

在解析世界中，总是有替代品。有些人宣誓依靠内置的'rexml'库或'oga'，另一个用于Ruby的XML/HTML解析器。但是Nokogiri因其健壮性和速度保持着人们的喜爱，更不用说其庞大的功能数组了。

在底层，Nokogiri将HTML转换为文档对象模型（DOM）——一种树状结构。这让导航和操纵元素变得简单。使用XPath和CSS选择器，您可以精确地指向您需要的任何信息。

## 另请参阅
- Nokogiri宝石：[https://nokogiri.org/](https://nokogiri.org/)
- Ruby的rexml文档：[https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- 替代解析器'oga'：[https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- 了解XPath：[https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
