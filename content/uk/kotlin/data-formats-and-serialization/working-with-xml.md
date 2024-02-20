---
date: 2024-01-26 04:33:49.702888-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0454 \u0442\u0430\u043A\u0456 \u043F\u0440\u043E\u0446\u0435\u0441\
  \u0438, \u044F\u043A \u0430\u043D\u0430\u043B\u0456\u0437, \u0441\u0442\u0432\u043E\
  \u0440\u0435\u043D\u043D\u044F \u0442\u0430 \u043C\u0430\u043D\u0456\u043F\u0443\
  \u043B\u044E\u0432\u0430\u043D\u043D\u044F XML-\u0434\u043E\u043A\u0443\u043C\u0435\
  \u043D\u0442\u0430\u043C\u0438 \u2014 \u043C\u043E\u0432\u0430 \u0440\u043E\u0437\
  \u043C\u0456\u0442\u043A\u0438 \u0434\u043B\u044F \u0437\u0431\u0435\u0440\u0456\
  \u0433\u0430\u043D\u043D\u044F \u0442\u0430 \u043F\u0435\u0440\u0435\u0434\u0430\
  \u0447\u0456 \u0434\u0430\u043D\u0438\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0456\u0441\u0442\u0438\u2026"
lastmod: 2024-02-19 22:05:08.284399
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0454 \u0442\u0430\u043A\u0456 \u043F\u0440\u043E\u0446\u0435\u0441\
  \u0438, \u044F\u043A \u0430\u043D\u0430\u043B\u0456\u0437, \u0441\u0442\u0432\u043E\
  \u0440\u0435\u043D\u043D\u044F \u0442\u0430 \u043C\u0430\u043D\u0456\u043F\u0443\
  \u043B\u044E\u0432\u0430\u043D\u043D\u044F XML-\u0434\u043E\u043A\u0443\u043C\u0435\
  \u043D\u0442\u0430\u043C\u0438 \u2014 \u043C\u043E\u0432\u0430 \u0440\u043E\u0437\
  \u043C\u0456\u0442\u043A\u0438 \u0434\u043B\u044F \u0437\u0431\u0435\u0440\u0456\
  \u0433\u0430\u043D\u043D\u044F \u0442\u0430 \u043F\u0435\u0440\u0435\u0434\u0430\
  \u0447\u0456 \u0434\u0430\u043D\u0438\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0456\u0441\u0442\u0438\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
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
