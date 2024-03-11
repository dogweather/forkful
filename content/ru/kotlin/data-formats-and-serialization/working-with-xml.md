---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:03.317943-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0440\u0430\u0437\u0431\
  \u043E\u0440, \u0441\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0438 \u043C\u0430\
  \u043D\u0438\u043F\u0443\u043B\u044F\u0446\u0438\u044E XML-\u0434\u043E\u043A\u0443\
  \u043C\u0435\u043D\u0442\u0430\u043C\u0438 \u2014 \u044F\u0437\u044B\u043A\u043E\
  \u043C \u0440\u0430\u0437\u043C\u0435\u0442\u043A\u0438 \u0434\u043B\u044F \u0445\
  \u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\u0434\
  \u0430\u0447\u0438 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E,\u2026"
lastmod: '2024-03-11T00:14:18.633621-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0440\u0430\u0437\u0431\
  \u043E\u0440, \u0441\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0438 \u043C\u0430\
  \u043D\u0438\u043F\u0443\u043B\u044F\u0446\u0438\u044E XML-\u0434\u043E\u043A\u0443\
  \u043C\u0435\u043D\u0442\u0430\u043C\u0438 \u2014 \u044F\u0437\u044B\u043A\u043E\
  \u043C \u0440\u0430\u0437\u043C\u0435\u0442\u043A\u0438 \u0434\u043B\u044F \u0445\
  \u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\u0434\
  \u0430\u0447\u0438 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E,\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
---

{{< edit_this_page >}}

## Что и Почему?
Работа с XML включает в себя разбор, создание и манипуляцию XML-документами — языком разметки для хранения и передачи данных. Программисты делают это, потому что многие системы до сих пор обмениваются данными в формате XML, и это необходимо для поддержки устаревшего обеспечения и интеграции с существующими технологиями.

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
