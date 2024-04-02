---
date: 2024-01-26 04:35:11.198239-07:00
description: "\u4F7F\u7528XML\u610F\u5473\u7740\u4F7F\u7528\u4EE3\u7801\u6765\u89E3\
  \u6790\u3001\u751F\u6210\u548C\u64CD\u4F5CXML\uFF08\u53EF\u6269\u5C55\u6807\u8BB0\
  \u8BED\u8A00\uFF09\u6587\u6863\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u4E0E\u8BB8\u591A\u4F7F\u7528XML\u4F5C\u4E3A\u901A\u7528\u8BED\u8A00\u7684\
  \u7F51\u7EDC\u670D\u52A1\u3001\u914D\u7F6E\u6587\u4EF6\u548C\u6570\u636E\u4EA4\u6362\
  \u683C\u5F0F\u8FDB\u884C\u4EA4\u4E92\u3002"
lastmod: '2024-03-13T22:44:48.401408-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528XML\u610F\u5473\u7740\u4F7F\u7528\u4EE3\u7801\u6765\u89E3\u6790\
  \u3001\u751F\u6210\u548C\u64CD\u4F5CXML\uFF08\u53EF\u6269\u5C55\u6807\u8BB0\u8BED\
  \u8A00\uFF09\u6587\u6863\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\
  \u4E0E\u8BB8\u591A\u4F7F\u7528XML\u4F5C\u4E3A\u901A\u7528\u8BED\u8A00\u7684\u7F51\
  \u7EDC\u670D\u52A1\u3001\u914D\u7F6E\u6587\u4EF6\u548C\u6570\u636E\u4EA4\u6362\u683C\
  \u5F0F\u8FDB\u884C\u4EA4\u4E92\u3002"
title: "\u5904\u7406XML"
weight: 40
---

## 什么 & 为什么？
使用XML意味着使用代码来解析、生成和操作XML（可扩展标记语言）文档。程序员这样做是为了与许多使用XML作为通用语言的网络服务、配置文件和数据交换格式进行交互。

## 如何操作：
让我们使用Ruby自带的REXML来解析一个XML片段：
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') { |element|
  puts "Name: #{element.attributes['name']}, Color: #{element.attributes['color']}"
}
```
输出：
```
Name: apple, Color: green
Name: banana, Color: yellow
```

生成XML也很直接：
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
XML输出：
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## 深入了解：
XML的根源可追溯到1990年代，作为一种简化的SGML子集用于网络文档。它虽然冗长但结构高度严密，这也是为什么它能持续使用。虽然不是唯一选择——JSON和YAML因其简单性而变得流行——但XML在许多企业和遗留系统中仍然占据重要地位。

Ruby提供了几种处理XML的方法。REXML是一个全Ruby库，易于上手。Nokogiri是一个包装了更快的C库的宝石(gem)，提供速度和额外的功能。如何选择？对于较小的任务先从REXML开始，如果需要更强的性能则转向Nokogiri。

在底层，解析XML是关于将字符串转换为DOM或SAX模型。DOM在内存中创建一棵树，而SAX则流式处理文档并在解析时触发事件。REXML提供这两种模型，但通常比Nokogiri使用的C扩展要慢。

## 另见：
- Ruby REXML文档：https://www.rubydoc.info/stdlib/rexml
- Nokogiri宝石：https://nokogiri.org/
- XML规范：https://www.w3.org/XML/
- SAX简介：https://www.saxproject.org/
- YAML与JSON与XML比较：https://www.upwork.com/resources/json-vs-xml-vs-yaml
