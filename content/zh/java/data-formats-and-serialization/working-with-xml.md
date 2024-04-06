---
date: 2024-01-26 04:32:39.557927-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Java\u63D0\u4F9B\u4E86\u5982DOM\uFF08\
  \u6587\u6863\u5BF9\u8C61\u6A21\u578B\uFF09\u3001SAX\uFF08\u7B80\u5355API\u7528\u4E8E\
  XML\uFF09\u548CStAX\uFF08\u6D41API\u7528\u4E8EXML\uFF09\u7B49API\u6765\u5904\u7406\
  XML\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528DOM\u6765\u89E3\u6790XML\u6587\
  \u4EF6\u7684\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T22:38:46.812118-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Java\u63D0\u4F9B\u4E86\u5982DOM\uFF08\u6587\
  \u6863\u5BF9\u8C61\u6A21\u578B\uFF09\u3001SAX\uFF08\u7B80\u5355API\u7528\u4E8EXML\uFF09\
  \u548CStAX\uFF08\u6D41API\u7528\u4E8EXML\uFF09\u7B49API\u6765\u5904\u7406XML\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528DOM\u6765\u89E3\u6790XML\u6587\u4EF6\u7684\
  \u793A\u4F8B\uFF1A."
title: "\u5904\u7406XML"
weight: 40
---

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
