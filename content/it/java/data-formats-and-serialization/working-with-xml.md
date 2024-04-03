---
date: 2024-01-26 04:32:43.292912-07:00
description: "Lavorare con XML comporta il parsing, l'interrogazione e la manipolazione\
  \ di documenti XML con Java. I programmatori lo fanno per lo scambio di dati, la\u2026"
lastmod: '2024-03-13T22:44:43.333309-06:00'
model: gpt-4-0125-preview
summary: Lavorare con XML comporta il parsing, l'interrogazione e la manipolazione
  di documenti XML con Java.
title: Lavorare con XML
weight: 40
---

## Cosa & Perché?
Lavorare con XML comporta il parsing, l'interrogazione e la manipolazione di documenti XML con Java. I programmatori lo fanno per lo scambio di dati, la gestione della configurazione e perché molti sistemi legacy e API comunicano utilizzando XML.

## Come fare:
Java fornisce API come DOM (Document Object Model), SAX (Simple API for XML) e StAX (Streaming API for XML) per lavorare con XML. Ecco un esempio DOM per analizzare un file XML:

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
            NodeList nodoLista = doc.getElementsByTagName("employee");
            
            for (int i = 0; i < nodoLista.getLength(); i++) {
                Element elemento = (Element) nodoLista.item(i);
                System.out.println("Nome: " + elemento.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Età: " + elemento.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Supponiamo che `data.xml` sia così:

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

L'output sarebbe:

```
Nome: Jane Doe
Età: 30
Nome: John Doe
Età: 40
```

## Approfondimento
XML esiste dalla fine degli anni '90, fornendo un modo strutturato e flessibile per lo scambio di dati tra diversi sistemi. Anche se JSON è diventato più popolare per le nuove API web a causa della sua sintassi più semplice e della stretta integrazione con JavaScript, XML rimane ampiamente utilizzato negli ambienti aziendali, nei servizi web basati su SOAP e negli standard di documenti come Office Open XML per Microsoft Office.

Per quanto riguarda il parsing di XML in Java, l'API DOM è ottima per documenti più piccoli: è basata su alberi e consente un accesso completo alla struttura XML in memoria. Tuttavia, per file più grandi, può essere intensivo in termini di memoria. SAX e StAX sono più amichevoli della memoria, poiché sono basati su eventi e su flusso rispettivamente, ma possono essere meno comodi per navigare nelle strutture XML.

Per creare o modificare XML, Java fornisce anche i pacchetti javax.xml.transform e javax.xml.bind (JAXB). JAXB faceva parte di Java SE fino alla versione 10, in seguito è diventato una libreria separata a causa della rimozione dei moduli Java EE da Java SE. È un modo guidato dalle annotazioni per serializzare oggetti Java in XML e viceversa.

## Vedi Anche
Consulta queste fonti correlate per saperne di più sul lavoro con XML in Java:
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java Architecture for XML Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Guida Oracle all'XML in Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [Tecnologia XML del W3C](https://www.w3.org/standards/xml/)
- [Stack Overflow: Domande taggate 'java' e 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
