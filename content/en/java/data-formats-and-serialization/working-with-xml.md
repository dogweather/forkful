---
date: 2024-01-25 03:39:39.784719-07:00
description: "Working with XML entails parsing, querying, and manipulating XML documents\
  \ with Java. Programmers do it for data interchange, configuration management,\u2026"
lastmod: '2024-03-13T22:44:59.993506-06:00'
model: gpt-4-1106-preview
summary: Working with XML entails parsing, querying, and manipulating XML documents
  with Java.
title: Working with XML
weight: 40
---

## What & Why?
Working with XML entails parsing, querying, and manipulating XML documents with Java. Programmers do it for data interchange, configuration management, and because many legacy systems and APIs communicate using XML.

## How to:
Java provides APIs like DOM (Document Object Model), SAX (Simple API for XML), and StAX (Streaming API for XML) to work with XML. Here's a DOM example to parse an XML file:

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
                System.out.println("Name: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Age: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Suppose `data.xml` looks like this:

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

The output would be:

```
Name: Jane Doe
Age: 30
Name: John Doe
Age: 40
```

## Deep Dive
XML has been around since the late '90s, providing a structured and flexible way to exchange data across different systems. Although JSON has become more popular for new web APIs due to its simpler syntax and tight integration with JavaScript, XML remains widely used in enterprise environments, SOAP-based web services, and document standards like Office Open XML for Microsoft Office.

When it comes to parsing XML in Java, the DOM API is great for smaller documents: it's tree-based and allows full access to the XML structure in memory. However, for larger files, it can be memory-intensive. SAX and StAX are more memory-friendly, as they are event-driven and stream-based respectively, but they can be less convenient for navigating XML structures.

For creating or modifying XML, Java also provides the javax.xml.transform and javax.xml.bind (JAXB) packages. JAXB was part of Java SE until version 10, afterwards, it became a separate library due to the Java EE modules' removal from Java SE. It's an annotation-driven way to serialize Java objects to XML and vice versa.

## See Also
Check out these related sources for more on working with XML in Java:
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java Architecture for XML Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Oracle's Guide to XML in Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XML Technology](https://www.w3.org/standards/xml/)
- [Stack Overflow: Questions tagged 'java' and 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
