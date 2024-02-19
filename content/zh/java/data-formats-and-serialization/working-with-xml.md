---
aliases:
- /zh/java/working-with-xml/
date: 2024-01-26 04:32:39.557927-07:00
description: "\u4F7F\u7528Java\u5904\u7406XML\u6D89\u53CA\u5230\u89E3\u6790\u3001\u67E5\
  \u8BE2\u548C\u64CD\u4F5CXML\u6587\u6863\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\
  \u8FD9\u6837\u505A\uFF0C\u662F\u4E3A\u4E86\u6570\u636E\u4EA4\u6362\u3001\u914D\u7F6E\
  \u7BA1\u7406\uFF0C\u4EE5\u53CA\u8BB8\u591A\u9057\u7559\u7CFB\u7EDF\u548CAPI\u4F7F\
  \u7528XML\u8FDB\u884C\u901A\u4FE1\u3002"
lastmod: 2024-02-18 23:08:59.042847
model: gpt-4-0125-preview
summary: "\u4F7F\u7528Java\u5904\u7406XML\u6D89\u53CA\u5230\u89E3\u6790\u3001\u67E5\
  \u8BE2\u548C\u64CD\u4F5CXML\u6587\u6863\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\
  \u8FD9\u6837\u505A\uFF0C\u662F\u4E3A\u4E86\u6570\u636E\u4EA4\u6362\u3001\u914D\u7F6E\
  \u7BA1\u7406\uFF0C\u4EE5\u53CA\u8BB8\u591A\u9057\u7559\u7CFB\u7EDF\u548CAPI\u4F7F\
  \u7528XML\u8FDB\u884C\u901A\u4FE1\u3002"
title: "\u5904\u7406XML"
---

{{< edit_this_page >}}

## 什么与为什么？
使用Java处理XML涉及到解析、查询和操作XML文档。程序员之所以这样做，是为了数据交换、配置管理，以及许多遗留系统和API使用XML进行通信。

## 如何操作：
Java提供了如DOM（文档对象模型）、SAX（简单API用于XML）和StAX（流API用于XML）等API来处理XML。这里有一个使用DOM来解析XML文件的示例：

```java
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class XmlParser {
    public static void main(String[] args) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse("data.xml");

            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("employee");

            for (int i = 0; i < nodeList.getLength(); i++) {
                Element element = (Element) nodeList.item(i);
                System.out.println("姓名：" + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("年龄：" + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

假设`data.xml`看起来是这样的：

```xml
<employees>
    <employee>
        <name>Jane Doe</name>
        <age>30</age>
    </employee>
    <employee>
        <name>John Doe</name>
        <age>40</age>
    </employee>
</employees>
```

输出将会是：

```
姓名：Jane Doe
年龄：30
姓名：John Doe
年龄：40
```

## 深入了解
XML自90年代末就开始使用了，为不同系统之间提供了一种结构化且灵活的数据交换方式。虽然由于JSON具有更简单的语法和与JavaScript的紧密集成，它已经变得更适用于新的网络API，但XML在企业环境、基于SOAP的web服务和像Office Open XML这样的文档标准中仍被广泛使用。

当涉及到在Java中解析XML时，DOM API非常适合较小的文档：它是基于树的，允许完全访问内存中的XML结构。然而，对于较大的文件，它可能会占用大量内存。SAX和StAX对内存更友好，因为它们是基于事件和基于流的，但是它们在导航XML结构时可能会不太方便。

对于创建或修改XML，Java还提供了javax.xml.transform和javax.xml.bind（JAXB）包。JAXB在Java SE的版本10之前是Java SE的一部分，此后，由于Java SE从Java EE模块中移除，它成为了一个独立的库。它是一种通过注解来驱动的方式，用于将Java对象序列化为XML，反之亦然。

## 另请参阅
查看这些相关资源，以获取更多关于在Java中处理XML的信息：
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java Architecture for XML Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Oracle 对于Java中XML的指南](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XML Technology](https://www.w3.org/standards/xml/)
- [Stack Overflow: 标有'java' 和 'xml'的问题](https://stackoverflow.com/questions/tagged/java+xml)
