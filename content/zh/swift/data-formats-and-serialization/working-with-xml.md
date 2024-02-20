---
date: 2024-01-26 04:36:10.513673-07:00
description: null
lastmod: 2024-02-19 22:05:07.246627
model: gpt-4-0125-preview
summary: null
title: "\u5904\u7406XML"
---

{{< edit_this_page >}}

# 什么 & 为什么?
在 Swift 中处理 XML 意味着解析和生成 XML 数据。程序员这么做是为了数据交换，特别是当与采用 XML 作为标准格式的系统集成时。

# 如何操作:
Swift 提供了 `XMLParser` 和 `XMLDocument` 来解析 XML 数据。这里是一个解析简单 XML 字符串的代码片段：

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Don't forget the party on Friday!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // 您的 XMLParserDelegate
    parser.parse()
}
```

您还可以使用 `XMLDocument` 生成 XML：

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

样本输出：

```xml
<note>
  <to>Tove</to>
</note>
```

# 深入探讨
自 90 年代末以来，XML（扩展标记语言）已经存在。它冗长但可读性强，使其适合复杂的数据结构。与 Python 的 ElementTree 或 Java 的 JAXB 相比，Swift 的 XML 解析能力可能不那么强大，但对于基本需求来说足够了。

由于 JSON 的体积更轻且解析器较简单，新系统中常常偏爱使用 JSON，但 XML 在许多企业和遗产系统中仍然占有一席之地。

在 Swift 中处理 XML 时，`XMLParser` 是基于流的解析器，这意味着它会顺序读取 XML 文档。对于大型 XML 文件，这样做在内存效率上有优势。然而，如果您寻求的是简单性，并且您的 XML 数据相对较小，使用 `XMLDocument` 可能更直接。

# 参考资料
- [苹果的 XML 解析指南](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schools XML 教程](https://www.w3schools.com/xml/)
