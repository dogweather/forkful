---
date: 2024-01-26 04:33:49.702888-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0423\
  \ Kotlin \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u0456 `javax.xml.parsers` \u0434\u043B\u044F \u043F\
  \u0430\u0440\u0441\u0438\u043D\u0433\u0443."
lastmod: '2024-03-13T22:44:49.259768-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Kotlin \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0432\
  \u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0456 `javax.xml.parsers` \u0434\u043B\
  \u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Як це робити:
У Kotlin ви можете використовувати вбудовані `javax.xml.parsers` для парсингу:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
Для створення XML-документів можна використовувати `javax.xml.transform`:

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
Приклад виводу для перетворення документа у рядок просто буде вашим XML-вмістом у форматі рядка.

## Погляд углиб
XML був каменем спотикання у розробці вебу та програмного забезпечення з 90-х років, заслуживши визнання завдяки своїй читабельності та структурованій ієрархії. Хоча JSON набув популярності для веб-сервісів завдяки своїй простоті та меншому розміру повідомлень, XML залишається поширеним у корпоративному середовищі, веб-сервісах на основі SOAP та конфігураціях (наприклад, у файлах макету Android).

Є різні бібліотеки та API, крім вбудованих функцій Kotlin/Java для роботи з XML, такі як Simple XML Serialization та Jackson XML module. Але `javax.xml.parsers` та `javax.xml.transform` зазвичай задовольняють більшість потреб без додавання зовнішніх залежностей.

При роботі з XML у Kotlin ключові деталі реалізації включають належне вирішення проблем кодування символів і управління XML-ентітетами, щоб запобігти атакам через ін'єкцію XML. Будьте уважні до складностей простору імен та перевірки схеми під час аналізу XML, щоб забезпечити цілісність даних.

## Дивіться також
- [Документація Kotlin](https://kotlinlang.org/docs/reference/)
- [Документація Java DOM](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Модуль XML Jackson](https://github.com/FasterXML/jackson-dataformat-xml)
