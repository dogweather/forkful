---
date: 2024-01-26 04:33:05.901586-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043F\u0435\u0440\u0435\
  \u0434\u0431\u0430\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437, \u0437\
  \u0430\u043F\u0438\u0442\u0443\u0432\u0430\u043D\u043D\u044F \u0442\u0430 \u043C\
  \u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u043D\u043D\u044F \u0434\u043E\
  \u043A\u0443\u043C\u0435\u043D\u0442\u0430\u043C\u0438 XML \u0437\u0430 \u0434\u043E\
  \u043F\u043E\u043C\u043E\u0433\u043E\u044E Java. \u041F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\
  \u0435 \u0434\u043B\u044F \u043E\u0431\u043C\u0456\u043D\u0443 \u0434\u0430\u043D\
  \u0438\u043C\u0438, \u0443\u043F\u0440\u0430\u0432\u043B\u0456\u043D\u043D\u044F\
  \u2026"
lastmod: '2024-03-13T22:44:49.120907-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043F\u0435\u0440\u0435\
  \u0434\u0431\u0430\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437, \u0437\
  \u0430\u043F\u0438\u0442\u0443\u0432\u0430\u043D\u043D\u044F \u0442\u0430 \u043C\
  \u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u043D\u043D\u044F \u0434\u043E\
  \u043A\u0443\u043C\u0435\u043D\u0442\u0430\u043C\u0438 XML \u0437\u0430 \u0434\u043E\
  \u043F\u043E\u043C\u043E\u0433\u043E\u044E Java."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

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
