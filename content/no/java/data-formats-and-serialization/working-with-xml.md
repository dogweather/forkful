---
title:                "Å jobbe med XML"
aliases:
- /no/java/working-with-xml.md
date:                  2024-01-26T04:32:38.768670-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med XML innebærer parsing, spørring og manipulering av XML-dokumenter med Java. Programmerere gjør dette for datautveksling, konfigurasjonsstyring og fordi mange eldre systemer og API-er kommuniserer ved hjelp av XML.

## Hvordan:
Java tilbyr API-er som DOM (Document Object Model), SAX (Simple API for XML) og StAX (Streaming API for XML) for å jobbe med XML. Her er et DOM-eksempel for å parse en XML-fil:

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
                System.out.println("Navn: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Alder: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Anta at `data.xml` ser slik ut:

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

Utskriften vil være:

```
Navn: Jane Doe
Alder: 30
Navn: John Doe
Alder: 40
```

## Dypdykk
XML har vært rundt siden slutten av 90-tallet og tilbyr en strukturert og fleksibel måte å utveksle data på tvers av forskjellige systemer. Selv om JSON har blitt mer populært for nye web-API-er på grunn av sin enklere syntaks og tett integrasjon med JavaScript, forblir XML mye brukt i bedriftsmiljøer, SOAP-baserte webtjenester og dokumentstandarder som Office Open XML for Microsoft Office.

Når det kommer til parsing av XML i Java, er DOM-API-en flott for mindre dokumenter: den er basert på et tre og gir full tilgang til XML-strukturen i minnet. Imidlertid kan den være minnekrevende for større filer. SAX og StAX er mer minnevennlige, da de er hendelsesdrevne og strømbaserte henholdsvis, men de kan være mindre praktiske for å navigere XML-strukturer.

For å opprette eller modifisere XML, tilbyr Java også javax.xml.transform og javax.xml.bind (JAXB)-pakkene. JAXB var en del av Java SE frem til versjon 10, etterpå ble det et separat bibliotek på grunn av fjerningen av Java EE-moduler fra Java SE. Det er en annotasjonsdrevet måte å serialisere Java-objekter til XML og omvendt på.

## Se Også
Sjekk ut disse relaterte kildene for mer om å jobbe med XML i Java:
- [Java API for XML-behandling (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java-arkitektur for XML-binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Oracles guide til XML i Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XML-teknologi](https://www.w3.org/standards/xml/)
- [Stack Overflow: Spørsmål merket 'java' og 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
