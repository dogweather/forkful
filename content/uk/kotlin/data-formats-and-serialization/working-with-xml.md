---
title:                "Робота з XML"
aliases:
- /uk/kotlin/working-with-xml.md
date:                  2024-01-26T04:33:49.702888-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-xml.md"
---

{{< edit_this_page >}}

## Що та Чому?
Робота з XML включає такі процеси, як аналіз, створення та маніпулювання XML-документами — мова розмітки для зберігання та передачі даних. Програмісти роблять це, оскільки багато систем досі обмінюються даними в форматі XML, і це потрібно для підтримки спадщини та інтеграції з існуючими технологіями.

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
