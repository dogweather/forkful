---
date: 2024-01-26 04:36:17.320348-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u0432 Swift \u043E\u0437\
  \u043D\u0430\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437 \u0442\u0430\
  \ \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u044E \u0434\u0430\u043D\u0438\
  \u0445 XML. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\
  \u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u043E\u0431\
  \u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438, \u043E\u0441\u043E\
  \u0431\u043B\u0438\u0432\u043E \u043A\u043E\u043B\u0438 \u0456\u043D\u0442\u0435\
  \u0433\u0440\u0443\u044E\u0442\u044C\u0441\u044F \u0437 \u0441\u0438\u0441\u0442\
  \u0435\u043C\u0430\u043C\u0438, \u0434\u0435 XML \u0454\u2026"
lastmod: '2024-03-13T22:44:49.960858-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u0432 Swift \u043E\u0437\
  \u043D\u0430\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437 \u0442\u0430\
  \ \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u044E \u0434\u0430\u043D\u0438\
  \u0445 XML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Що та Чому?
Робота з XML в Swift означає аналіз та генерацію даних XML. Програмісти роблять це для обміну даними, особливо коли інтегруються з системами, де XML є стандартним форматом.

## Як це зробити:
Swift надає `XMLParser` та `XMLDocument` для аналізу даних XML. Ось приклад аналізу простого XML рядка:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Не забудь про вечірку в п'ятницю!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Ваш XMLParserDelegate
    parser.parse()
}
```

Ви також можете генерувати XML за допомогою `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Приклад виводу:

```xml
<note>
  <to>Tove</to>
</note>
```

## Поглиблений огляд
XML або Розширювана Мова Розмітки (Extensible Markup Language) існує з кінця 90-х. Він має багатослівний, але легкий для читання формат, що робить його підходящим для складних структур даних. Можливості аналізу XML в Swift не настільки потужні, як у ElementTree Python або JAXB Java, але вони достатні для базових потреб.

Альтернативи, наприклад JSON, часто віддають перевагу в нових системах через їх меншу складність та легші парсери, але XML все ще поширений в багатьох корпоративних та усталених системах.

При роботі з XML у Swift, `XMLParser` є парсером, заснованим на потоках, що означає, що він послідовно читає документ XML. Для великих файлів XML це ефективно за пам'яттю. Проте, якщо ви шукаєте простоту і ваші дані XML є відносно невеликими, використання `XMLDocument` може бути прямолінійнішим.

## См. також
- [Посібник з аналізу XML від Apple](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [Посібник з XML від W3Schools](https://www.w3schools.com/xml/)
