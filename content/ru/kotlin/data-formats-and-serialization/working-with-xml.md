---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:03.317943-07:00
description: "\u041A\u0430\u043A: \u0412 Kotlin \u0434\u043B\u044F \u0440\u0430\u0437\
  \u0431\u043E\u0440\u0430 \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0432\u0441\u0442\
  \u0440\u043E\u0435\u043D\u043D\u044B\u0439 `javax.xml.parsers`."
lastmod: '2024-03-13T22:44:45.023377-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Kotlin \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430\
  \ \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u044B\u0439 `javax.xml.parsers`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Как:
В Kotlin для разбора вы можете использовать встроенный `javax.xml.parsers`:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
Для создания XML-документов вы можете использовать `javax.xml.transform`:

```Kotlin
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Document
import java.io.StringWriter

fun convertDocumentToString(doc: Document): String {
    val transformer = TransformerFactory.newInstance().newTransformer()
    val result = StringWriter()
    transformer.transform(DOMSource(doc), StreamResult(result))
    return result.toString()
}
```
Пример вывода для преобразования документа в строку будет просто вашим XML-контентом в формате строки.

## Погружение
XML был ключевым элементом веб- и программной разработки с 90-х годов, получивший распространение благодаря своей читаемости и структурированной иерархии. Хотя JSON набрал популярность для веб-служб из-за своей простоты и меньшего размера сообщения, XML остается распространенным в корпоративных средах, веб-службах на основе SOAP и конфигурациях (например, в файлах макета Android).

Существуют различные библиотеки и API помимо встроенных возможностей Kotlin/Java для работы с XML, такие как Simple XML Serialization и модуль Jackson XML. Но `javax.xml.parsers` и `javax.xml.transform` обычно удовлетворяют большинство потребностей без добавления внешних зависимостей.

При работе с XML в Kotlin ключевые детали реализации включают в себя правильную обработку кодировки символов и управление XML-сущностями для предотвращения атак через инъекцию XML. Будьте внимательны к сложностям пространств имён и проверке схемы при разборе XML для обеспечения целостности данных.

## Смотрите также
- [Документация Kotlin](https://kotlinlang.org/docs/reference/)
- [Документация Java DOM](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Модуль Jackson XML](https://github.com/FasterXML/jackson-dataformat-xml)
