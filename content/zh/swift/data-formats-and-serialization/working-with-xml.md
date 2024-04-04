---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-25 03:39:31.391206-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Swift \u63D0\u4F9B\u4E86 `XMLParser` \u548C\
  \ `XMLDocument` \u6765\u89E3\u6790XML\u6570\u636E\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\
  \u89E3\u6790\u7B80\u5355XML\u5B57\u7B26\u4E32\u7684\u4EE3\u7801\u7247\u6BB5\uFF1A\
  ."
lastmod: '2024-04-04T00:26:47.117612-06:00'
model: gpt-4-0125-preview
summary: "Swift \u63D0\u4F9B\u4E86 `XMLParser` \u548C `XMLDocument` \u6765\u89E3\u6790\
  XML\u6570\u636E\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u89E3\u6790\u7B80\u5355XML\u5B57\
  \u7B26\u4E32\u7684\u4EE3\u7801\u7247\u6BB5\uFF1A."
title: "\u5904\u7406XML"
weight: 40
---

## 如何操作:

Swift 提供了 `XMLParser` 和 `XMLDocument` 来解析XML数据。这里有一个解析简单XML字符串的代码片段：

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>别忘了周五的派对！</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // 你的XMLParserDelegate
    parser.parse()
}
```

你也可以使用 `XMLDocument` 来生成XML：

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

示例输出：

```xml
<note>
  <to>Tove</to>
</note>
```

## 深入探索
XML，或可扩展标记语言，自90年代末以来就已存在。它冗长但人类可读，使其适合复杂数据结构。Swift 的 XML 解析功能不如 Python 的 ElementTree 或 Java 的 JAXB 那样强大，但对于基本需求来说已经足够了。

由于它们的轻量级和解析器不那么复杂，新系统中常常更喜欢用像 JSON 这样的替代品，但 XML 在许多企业和遗留系统中仍然占有一席之地。

在 Swift 中处理 XML 时，`XMLParser` 是基于流的解析器，意味着它按顺序读取XML文档。对于大型 XML 文件，这种方式对内存是高效的。然而，如果你追求简单并且你的 XML 数据相对较小，使用 `XMLDocument` 可能会更直接。

## 另请参阅
- [Apple 的 XML 解析指南](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schools XML 教程](https://www.w3schools.com/xml/)
