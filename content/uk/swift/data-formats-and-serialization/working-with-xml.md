---
date: 2024-01-26 04:36:17.320348-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Swift \u043D\u0430\u0434\u0430\u0454 `XMLParser` \u0442\u0430 `XMLDocument` \u0434\
  \u043B\u044F \u0430\u043D\u0430\u043B\u0456\u0437\u0443 \u0434\u0430\u043D\u0438\
  \u0445 XML. \u041E\u0441\u044C \u043F\u0440\u0438\u043A\u043B\u0430\u0434 \u0430\
  \u043D\u0430\u043B\u0456\u0437\u0443 \u043F\u0440\u043E\u0441\u0442\u043E\u0433\u043E\
  \ XML \u0440\u044F\u0434\u043A\u0430."
lastmod: '2024-03-13T22:44:49.960858-06:00'
model: gpt-4-0125-preview
summary: "Swift \u043D\u0430\u0434\u0430\u0454 `XMLParser` \u0442\u0430 `XMLDocument`\
  \ \u0434\u043B\u044F \u0430\u043D\u0430\u043B\u0456\u0437\u0443 \u0434\u0430\u043D\
  \u0438\u0445 XML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

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
