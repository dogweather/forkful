---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:28.858512-07:00
description: "Werken met XML houdt het parsen, opvragen en manipuleren van XML-documenten\
  \ met Java in. Programmeurs doen dit voor gegevensuitwisseling,\u2026"
lastmod: '2024-03-13T22:44:50.706715-06:00'
model: gpt-4-0125-preview
summary: Werken met XML houdt het parsen, opvragen en manipuleren van XML-documenten
  met Java in.
title: Werken met XML
weight: 40
---

## Hoe te:
Java biedt API's zoals DOM (Document Object Model), SAX (Simple API for XML) en StAX (Streaming API for XML) om met XML te werken. Hier is een DOM-voorbeeld om een XML-bestand te parsen:

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
                System.out.println("Naam: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Leeftijd: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Stel je voor dat `data.xml` er zo uitziet:

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

De output zou zijn:

```
Naam: Jane Doe
Leeftijd: 30
Naam: John Doe
Leeftijd: 40
```

## Diepgaande duik
XML bestaat al sinds de late jaren '90 en biedt een gestructureerde en flexibele manier om gegevens uit te wisselen tussen verschillende systemen. Hoewel JSON populairder is geworden voor nieuwe web-API's vanwege de eenvoudigere syntaxis en strakke integratie met JavaScript, blijft XML veel gebruikt in enterprise-omgevingen, SOAP-gebaseerde webdiensten en documentstandaarden zoals Office Open XML voor Microsoft Office.

Wat betreft het parsen van XML in Java, de DOM API is geweldig voor kleinere documenten: het is op bomen gebaseerd en biedt volledige toegang tot de XML-structuur in het geheugen. Voor grotere bestanden kan het echter geheugenintensief zijn. SAX en StAX zijn meer geheugenvriendelijk omdat ze respectievelijk op gebeurtenissen gebaseerd en streamgebaseerd zijn, maar ze kunnen minder handig zijn voor het navigeren door XML-structuren.

Voor het creëren of wijzigen van XML biedt Java ook de javax.xml.transform en javax.xml.bind (JAXB) pakketten. JAXB was tot versie 10 deel van Java SE, daarna werd het een afzonderlijke bibliotheek vanwege de verwijdering van de Java EE-modules uit Java SE. Het is een op annotaties gebaseerde manier om Java-objecten te serialiseren naar XML en vice versa.

## Zie ook
Bekijk deze gerelateerde bronnen voor meer over het werken met XML in Java:
- [Java API voor XML-verwerking (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java-architectuur voor XML-binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Oracle's gids voor XML in Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XML-technologie](https://www.w3.org/standards/xml/)
- [Stack Overflow: Vragen getagd 'java' en 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
