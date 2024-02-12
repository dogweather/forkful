---
title:                "Praca z XML"
aliases: - /pl/java/working-with-xml.md
date:                  2024-01-26T04:32:44.616684-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-xml.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Praca z XML obejmuje parsowanie, kwerendowanie i manipulowanie dokumentami XML za pomocą Javy. Programiści robią to dla wymiany danych, zarządzania konfiguracją i ponieważ wiele systemów dziedzicznych oraz API komunikuje się przy użyciu XML.

## Jak to zrobić:
Java udostępnia takie API jak DOM (Document Object Model), SAX (Simple API for XML) i StAX (Streaming API for XML) do pracy z XML. Oto przykład użycia DOM do parsowania pliku XML:

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
                System.out.println("Imię: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Wiek: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Załóżmy, że `data.xml` wygląda tak:

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

Wyjście będzie wyglądać następująco:

```
Imię: Jane Doe
Wiek: 30
Imię: John Doe
Wiek: 40
```

## Głębsze spojrzenie
XML istnieje od końca lat 90., zapewniając strukturalny i elastyczny sposób na wymianę danych pomiędzy różnymi systemami. Chociaż JSON stał się bardziej popularny dla nowych interfejsów API sieciowych ze względu na jego prostszą składnię i ścisłą integrację z JavaScript, XML nadal jest powszechnie używany w środowiskach korporacyjnych, opartych na SOAP usługach sieciowych i standardach dokumentów jak Office Open XML dla Microsoft Office.

Jeśli chodzi o parsowanie XML w Javie, API DOM jest świetne dla mniejszych dokumentów: jest oparte na drzewie i pozwala na pełny dostęp do struktury XML w pamięci. Jednakże, dla większych plików, może być intensywne pod względem zużycia pamięci. SAX i StAX są bardziej przyjazne dla pamięci, ponieważ są oparte na zdarzeniach i strumieniowe odpowiednio, ale mogą być mniej wygodne do nawigacji po strukturach XML.

Do tworzenia lub modyfikowania XML, Java również udostępnia pakiety javax.xml.transform i javax.xml.bind (JAXB). JAXB był częścią Java SE do wersji 10, potem stał się oddzielną biblioteką z powodu usunięcia modułów Java EE z Java SE. Jest to sposób oparty na adnotacjach do serializacji obiektów Java do XML i odwrotnie.

## Zobacz również
Sprawdź te powiązane źródła, aby dowiedzieć się więcej o pracy z XML w Javie:
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java Architecture for XML Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Przewodnik Oracle po XML w Javie](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [Technologia XML W3C](https://www.w3.org/standards/xml/)
- [Stack Overflow: Pytania oznaczone 'java' i 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
