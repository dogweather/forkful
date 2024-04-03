---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:04.997869-07:00
description: "\u89E3\u6790HTML\u610F\u5473\u7740\u62C6\u5206\u4E00\u5927\u5757HTML\u4EE3\
  \u7801\u4EE5\u638C\u63E1\u5176\u7ED3\u6784\u548C\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u63D0\u53D6\u6570\u636E\u3001\u64CD\u7EB5\u5185\
  \u5BB9\u6216\u5728\u683C\u5F0F\u548C\u7CFB\u7EDF\u4E4B\u95F4\u8FC1\u79FB\u4FE1\u606F\
  \u3002"
lastmod: '2024-03-13T22:44:48.371196-06:00'
model: gpt-4-0125-preview
summary: "\u89E3\u6790HTML\u610F\u5473\u7740\u62C6\u5206\u4E00\u5927\u5757HTML\u4EE3\
  \u7801\u4EE5\u638C\u63E1\u5176\u7ED3\u6784\u548C\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u63D0\u53D6\u6570\u636E\u3001\u64CD\u7EB5\u5185\
  \u5BB9\u6216\u5728\u683C\u5F0F\u548C\u7CFB\u7EDF\u4E4B\u95F4\u8FC1\u79FB\u4FE1\u606F\
  \u3002."
title: "\u89E3\u6790HTML"
weight: 43
---

## 什么 & 为什么？
解析HTML意味着拆分一大块HTML代码以掌握其结构和内容。程序员这样做是为了提取数据、操纵内容或在格式和系统之间迁移信息。

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
