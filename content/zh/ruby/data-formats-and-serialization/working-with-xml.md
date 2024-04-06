---
date: 2024-01-26 04:35:11.198239-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8BA9\u6211\u4EEC\u4F7F\u7528Ruby\u81EA\
  \u5E26\u7684REXML\u6765\u89E3\u6790\u4E00\u4E2AXML\u7247\u6BB5\uFF1A."
lastmod: '2024-04-05T22:38:47.534410-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8BA9\u6211\u4EEC\u4F7F\u7528Ruby\u81EA\u5E26\
  \u7684REXML\u6765\u89E3\u6790\u4E00\u4E2AXML\u7247\u6BB5\uFF1A."
title: "\u5904\u7406XML"
weight: 40
---

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
