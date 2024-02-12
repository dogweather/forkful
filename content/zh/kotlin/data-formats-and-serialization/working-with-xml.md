---
title:                "处理XML"
aliases: - /zh/kotlin/working-with-xml.md
date:                  2024-01-26T04:33:22.547834-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
处理XML涉及到解析、创建和操作XML文档——一种用于数据存储和转移的标记语言。程序员之所以这么做是因为许多系统仍然在XML格式中交换数据，而且这对于遗留支持和与现有技术的集成是必需的。

## 如何操作:
在Kotlin中，您可以使用内置的`javax.xml.parsers`进行解析：

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
要创建XML文档，您可能会使用`javax.xml.transform`：

```Kotlin
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Document
import java.io.StringWriter

fun convertDocumentToString(doc: Document): String {
    val transformer = TransformerFactory.newInstance().newTransformer()
    val result = StringWriter()
    transformer.transform(DOMSource(doc), StreamResult(result))
    return result.toString()
}
```
将文档转换为字符串的示例输出只是将您的XML内容以字符串格式表示。

## 深入探讨
XML自90年代以来一直是网络和软件开发的基石，因其可读性和结构化层次而受到青睐。虽然由于简便性和较小的消息大小，JSON对于网络服务变得更加受欢迎，但XML在企业环境、基于SOAP的网络服务和配置中（如Android布局文件）仍然流行。

除了Kotlin/Java的内置功能之外，还有各种库和API用于处理XML，如Simple XML Serialization和Jackson XML模块。但`javax.xml.parsers`和`javax.xml.transform`通常足以满足大多数需求，而无需添加外部依赖。

在Kotlin中处理XML时，关键的实现细节包括正确处理字符编码和管理XML实体以防止XML注入攻击。在解析XML以确保数据完整性时要注意命名空间的复杂性和模式验证。

## 参见
- [Kotlin 文档](https://kotlinlang.org/docs/reference/)
- [Java DOM 文档](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML序列化](http://simple.sourceforge.net/)
- [Jackson XML 模块](https://github.com/FasterXML/jackson-dataformat-xml)
