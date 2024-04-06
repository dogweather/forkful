---
date: 2024-01-26 04:35:11.198239-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A XML\u7684\u6839\u6E90\u53EF\u8FFD\u6EAF\
  \u52301990\u5E74\u4EE3\uFF0C\u4F5C\u4E3A\u4E00\u79CD\u7B80\u5316\u7684SGML\u5B50\
  \u96C6\u7528\u4E8E\u7F51\u7EDC\u6587\u6863\u3002\u5B83\u867D\u7136\u5197\u957F\u4F46\
  \u7ED3\u6784\u9AD8\u5EA6\u4E25\u5BC6\uFF0C\u8FD9\u4E5F\u662F\u4E3A\u4EC0\u4E48\u5B83\
  \u80FD\u6301\u7EED\u4F7F\u7528\u3002\u867D\u7136\u4E0D\u662F\u552F\u4E00\u9009\u62E9\
  \u2014\u2014JSON\u548CYAML\u56E0\u5176\u7B80\u5355\u6027\u800C\u53D8\u5F97\u6D41\
  \u884C\u2014\u2014\u4F46XML\u5728\u8BB8\u591A\u4F01\u4E1A\u548C\u9057\u7559\u7CFB\
  \u7EDF\u4E2D\u4ECD\u7136\u5360\u636E\u91CD\u8981\u5730\u4F4D\u3002\u2026"
lastmod: '2024-04-05T22:51:01.597297-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u63D0\u4F9B\u4E86\u51E0\u79CD\u5904\u7406XML\u7684\u65B9\u6CD5\u3002\
  REXML\u662F\u4E00\u4E2A\u5168Ruby\u5E93\uFF0C\u6613\u4E8E\u4E0A\u624B\u3002Nokogiri\u662F\
  \u4E00\u4E2A\u5305\u88C5\u4E86\u66F4\u5FEB\u7684C\u5E93\u7684\u5B9D\u77F3(gem)\uFF0C\
  \u63D0\u4F9B\u901F\u5EA6\u548C\u989D\u5916\u7684\u529F\u80FD\u3002\u5982\u4F55\u9009\
  \u62E9\uFF1F\u5BF9\u4E8E\u8F83\u5C0F\u7684\u4EFB\u52A1\u5148\u4ECEREXML\u5F00\u59CB\
  \uFF0C\u5982\u679C\u9700\u8981\u66F4\u5F3A\u7684\u6027\u80FD\u5219\u8F6C\u5411Nokogiri\u3002"
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
