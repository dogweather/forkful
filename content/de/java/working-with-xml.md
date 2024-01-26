---
title:                "Arbeiten mit XML"
date:                  2024-01-26T04:32:21.203411-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit XML umfasst das Parsen, Abfragen und Manipulieren von XML-Dokumenten mit Java. Programmierer tun dies für den Datenaustausch, die Konfigurationsverwaltung und weil viele Alt-Systeme und APIs mittels XML kommunizieren.

## Wie zu:
Java bietet APIs wie DOM (Document Object Model), SAX (Simple API for XML) und StAX (Streaming API for XML), um mit XML zu arbeiten. Hier ist ein DOM-Beispiel zum Parsen einer XML-Datei:

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
                System.out.println("Alter: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Nehmen wir an, `data.xml` sieht so aus:

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

Die Ausgabe wäre:

```
Name: Jane Doe
Alter: 30
Name: John Doe
Alter: 40
```

## Tiefergehender Einblick
XML gibt es seit Ende der 90er Jahre und bietet eine strukturierte und flexible Methode für den Datenaustausch zwischen verschiedenen Systemen. Obwohl JSON aufgrund seiner einfacheren Syntax und engen Integration mit JavaScript für neue Web APIs beliebter geworden ist, bleibt XML in Unternehmensumgebungen, SOAP-basierte Webdienste und Dokumentstandards wie Office Open XML für Microsoft Office weit verbreitet.

Wenn es um das Parsen von XML in Java geht, ist die DOM API großartig für kleinere Dokumente: Sie basiert auf einem Baum und ermöglicht vollen Zugriff auf die XML-Struktur im Speicher. Für größere Dateien kann sie jedoch speicherintensiv sein. SAX und StAX sind speicherfreundlicher, da sie ereignisgesteuert bzw. auf Streams basieren, aber sie können weniger bequem sein für die Navigation in XML-Strukturen.

Für das Erstellen oder Modifizieren von XML bietet Java auch die Pakete javax.xml.transform und javax.xml.bind (JAXB). JAXB war bis Version 10 Teil von Java SE, danach wurde es aufgrund der Entfernung der Java EE Module aus Java SE zu einer separaten Bibliothek. Es ist eine annotierungsgesteuerte Methode, um Java-Objekte in XML zu serialisieren und umgekehrt.

## Siehe auch
Schauen Sie sich diese verwandten Quellen an, um mehr über die Arbeit mit XML in Java zu erfahren:
- [Java API für XML-Verarbeitung (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java-Architektur für XML-Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Oracles Leitfaden zu XML in Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XML-Technologie](https://www.w3.org/standards/xml/)
- [Stack Overflow: Fragen mit den Tags 'java' und 'xml'](https://stackoverflow.com/questions/tagged/java+xml)