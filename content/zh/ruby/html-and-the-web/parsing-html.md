---
title:                "解析HTML"
aliases:
- /zh/ruby/parsing-html.md
date:                  2024-02-03T19:13:04.997869-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
