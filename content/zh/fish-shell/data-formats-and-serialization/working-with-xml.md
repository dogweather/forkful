---
title:                "处理XML"
aliases:
- /zh/fish-shell/working-with-xml/
date:                  2024-01-26T04:31:03.617718-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
处理 XML 意味着在一个普遍存在的、结构化的格式中整理数据，这种格式用于配置、通信等多个方面。程序员操纵 XML 来读取、写入、更新和查询数据——这对于数以万计的应用程序和服务中的互操作性至关重要。

## 如何操作：
Fish 没有内置的 XML 解析功能，因此你需要依赖外部工具如 `xmllint` 或 `xmlstarlet`。以下是一个读取值的代码片段：

```fish
# 使用 xmlstarlet 解析 XML
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

输出：
```
Hello World
```

要编辑 XML，使用这个：

```fish
# 使用 xmlstarlet 编辑 XML 元素
echo '<root><element>Old Value</element></root>' | xmlstarlet ed -u "/root/element" -v 'New Value'
```

输出：
```xml
<?xml version="1.0"?>
<root>
  <element>New Value</element>
</root>
```

## 深入探讨：
XML 自 90 年代末期以来就存在了，它被设计为易读和对机器友好。虽然 JSON 因其简单性而取代了一些 XML 的热度，但在文档验证和命名空间至关重要的地方，XML 仍然根深蒂固。

有替代方案吗？当然——JSON、YAML，或甚至是像 Protocol Buffers 这样的二进制格式，适用于那些性能密集型的应用程序。但是，XML 的模式和 XSLT（用于 XML 转换）可以是复杂场景中关注稳健性时的决定性因素。

在底层，像 `xmlstarlet` 这样的工具包装了强大的库，比如 libxml2，为你提供 XPath 和 XQuery 以进行细致的 XML 调整。这些不仅仅是 XML 工具，而是通往 DOM 操作的大门，因为你会在任何处理 XML 的语言中应用类似的概念。

## 另请参见：
- [xmlstarlet 文档](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Fish 文档](https://fishshell.com/docs/current/index.html)
- [XPath 和 XQuery 函数与操作符](https://www.w3.org/TR/xpath-functions/)
