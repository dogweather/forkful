---
title:                "Att arbeta med XML"
date:                  2024-01-26T04:32:36.938542-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med XML"

category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-xml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att analysera, fråga och manipulera XML-dokument med Java. Programmerare gör detta för datautbyte, konfigurationshantering och eftersom många äldre system och API:er kommunicerar med XML.

## Hur man gör:
Java erbjuder API:er som DOM (Document Object Model), SAX (Simple API for XML) och StAX (Streaming API for XML) för att arbeta med XML. Här är ett DOM-exempel för att analysera en XML-fil:

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
                System.out.println("Namn: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Ålder: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Anta att `data.xml` ser ut så här:

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

Utmatningen blir:

```
Namn: Jane Doe
Ålder: 30
Namn: John Doe
Ålder: 40
```

## Djupdykning
XML har funnits sedan sent 90-tal, och tillhandahåller ett strukturerat och flexibelt sätt att utbyta data mellan olika system. Även om JSON har blivit mer populärt för nya webb-API:er på grund av dess enklare syntax och tät integration med JavaScript, används XML fortfarande flitigt i företagsmiljöer, SOAP-baserade webbtjänster och dokumentstandarder som Office Open XML för Microsoft Office.

När det kommer till att analysera XML i Java, är DOM API:et bra för mindre dokument: det är träd-baserat och tillåter full åtkomst till XML-strukturen i minnet. Dock kan det vara minneskrävande för större filer. SAX och StAX är mer minnesvänliga eftersom de är händelsedrivna och strömbaserade respektive, men de kan vara mindre bekväma för att navigera XML-strukturer.

För att skapa eller modifiera XML, tillhandahåller Java även paketen javax.xml.transform och javax.xml.bind (JAXB). JAXB var en del av Java SE till och med version 10, därefter blev det ett separat bibliotek på grund av borttagningen av Java EE-moduler från Java SE. Det är ett annotation-drivet sätt att serialisera Java-objekt till XML och vice versa.

## Se även
Kolla in dessa relaterade källor för mer om att arbeta med XML i Java:
- [Java API för XML-behandling (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java-arkitektur för XML-bindning (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Oracles guide till XML i Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XML-teknologi](https://www.w3.org/standards/xml/)
- [Stack Overflow: Frågor taggade 'java' och 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
