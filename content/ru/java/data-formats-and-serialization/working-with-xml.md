---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:18.948258-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0440\u0430\u0437\u0431\u043E\u0440, \u0437\u0430\u043F\
  \u0440\u043E\u0441 \u0438 \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u0435 XML \u0434\u043E\u043A\u0443\u043C\u0435\u043D\
  \u0442\u0430\u043C\u0438 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E Java.\
  \ \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\
  \u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043E\u0431\
  \u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\u0438, \u0443\u043F\
  \u0440\u0430\u0432\u043B\u0435\u043D\u0438\u044F\u2026"
lastmod: '2024-03-13T22:44:44.867569-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0440\u0430\u0437\u0431\u043E\u0440, \u0437\u0430\u043F\
  \u0440\u043E\u0441 \u0438 \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u0435 XML \u0434\u043E\u043A\u0443\u043C\u0435\u043D\
  \u0442\u0430\u043C\u0438 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E Java.\
  \ \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\
  \u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043E\u0431\
  \u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\u0438, \u0443\u043F\
  \u0440\u0430\u0432\u043B\u0435\u043D\u0438\u044F\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Что и Почему?
Работа с XML включает разбор, запрос и манипулирование XML документами с помощью Java. Программисты делают это для обмена данными, управления конфигурацией, а также потому, что многие устаревшие системы и API общаются с использованием XML.

## Как это сделать:
Java предлагает API, такие как DOM (Document Object Model, модель объекта документа), SAX (Simple API for XML, простой API для XML) и StAX (Streaming API for XML, потоковый API для XML), для работы с XML. Вот пример использования DOM для разбора XML файла:

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
                System.out.println("Имя: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Возраст: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Предположим, что `data.xml` выглядит так:

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

Вывод будет следующим:

```
Имя: Jane Doe
Возраст: 30
Имя: John Doe
Возраст: 40
```

## Подробнее
XML существует с конца 90-х годов, обеспечивая структурированный и гибкий способ обмена данными между разными системами. Хотя JSON стал более популярным для новых веб-API из-за его более простого синтаксиса и тесной интеграции с JavaScript, XML широко используется в корпоративных средах, веб-сервисах на основе SOAP и стандартах документов, например, Office Open XML для Microsoft Office.

Когда речь идет о разборе XML в Java, API DOM отлично подходит для меньших документов: он основан на дереве и позволяет полностью доступиться к структуре XML в памяти. Однако для больших файлов это может быть ресурсоемко. SAX и StAX более дружелюбны к памяти, так как они основаны на событиях и потоках соответственно, но могут быть менее удобны для навигации по структурам XML.

Для создания или изменения XML Java также предоставляет пакеты javax.xml.transform и javax.xml.bind (JAXB). JAXB был частью Java SE до версии 10, после чего стал отдельной библиотекой из-за удаления модулей Java EE из Java SE. Это аннотационный способ сериализации объектов Java в XML и наоборот.

## Смотрите также
Проверьте эти связанные источники для получения дополнительной информации о работе с XML в Java:
- [Java API для обработки XML (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Архитектура Java для привязки XML (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Руководство Oracle по XML в Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [Технологии XML от W3C](https://www.w3.org/standards/xml/)
- [Stack Overflow: Вопросы с тегами 'java' и 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
