---
title:                "Робота з XML"
aliases:
- /uk/java/working-with-xml.md
date:                  2024-01-26T04:33:05.901586-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з XML передбачає аналіз, запитування та маніпулювання документами XML за допомогою Java. Програмісти роблять це для обміну даними, управління конфігурацією, а також тому, що багато застарілих систем і API комунікують за допомогою XML.

## Як це робити:
Java надає API, як-от DOM (Document Object Model, Модель об'єкта документа), SAX (Simple API for XML, Простий API для XML) та StAX (Streaming API for XML, Потоковий API для XML), для роботи з XML. Ось приклад DOM для аналізу XML-файлу:

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
                System.out.println("Ім'я: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Вік: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Припустимо, `data.xml` виглядає ось так:

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

Вивід буде таким:

```
Ім'я: Jane Doe
Вік: 30
Ім'я: John Doe
Вік: 40
```

## Поглиблений розгляд
XML існує з кінця 90-х років, надаючи структурований і гнучкий спосіб обміну даними між різними системами. Хоча JSON став популярнішим для нових веб-API завдяки своєму простішому синтаксису і тісній інтеграції з JavaScript, XML залишається широко використовуваним в корпоративних середовищах, веб-сервісах на основі SOAP і стандартах документів, таких як Office Open XML для Microsoft Office.

У разі аналізу XML в Java, API DOM чудово підходить для менших документів: він базується на дереві і дозволяє повний доступ до структури XML у пам'яті. Однак, для більших файлів він може бути інтенсивним з точки зору використання пам'яті. SAX і StAX більш дружні до пам'яті, оскільки вони засновані на подіях та потоках відповідно, але можуть бути менш зручними для навігації по структурах XML.

Для створення або модифікації XML, Java також надає пакети javax.xml.transform і javax.xml.bind (JAXB). JAXB був частиною Java SE до версії 10, після чого він став окремою бібліотекою через видалення модулів Java EE з Java SE. Це спосіб серіалізації об'єктів Java в XML і навпаки, заснований на анотаціях.

## Дивіться також
Перегляньте ці пов'язані джерела для отримання додаткової інформації про роботу з XML у Java:
- [Java API для обробки XML (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Архітектура Java для зв'язування XML (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Посібник Oracle з XML у Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [Технологія XML W3C](https://www.w3.org/standards/xml/)
- [Stack Overflow: Питання з тегами 'java' та 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
