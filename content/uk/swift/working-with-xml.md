---
title:                "Робота з XML"
aliases:
- uk/swift/working-with-xml.md
date:                  2024-01-26T04:36:17.320348-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-xml.md"
---

{{< edit_this_page >}}

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
