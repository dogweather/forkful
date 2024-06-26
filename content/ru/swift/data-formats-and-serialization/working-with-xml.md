---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:13.441849-07:00
description: "\u041A\u0430\u043A: Swift \u043F\u0440\u0435\u0434\u043E\u0441\u0442\
  \u0430\u0432\u043B\u044F\u0435\u0442 `XMLParser` \u0438 `XMLDocument` \u0434\u043B\
  \u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0434\u0430\u043D\u043D\u044B\
  \u0445 XML. \u0412\u043E\u0442 \u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442\
  \ \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u043F\u0440\u043E\
  \u0441\u0442\u043E\u0439 \u0441\u0442\u0440\u043E\u043A\u0438 XML."
lastmod: '2024-03-13T22:44:45.726840-06:00'
model: gpt-4-0125-preview
summary: "Swift \u043F\u0440\u0435\u0434\u043E\u0441\u0442\u0430\u0432\u043B\u044F\
  \u0435\u0442 `XMLParser` \u0438 `XMLDocument` \u0434\u043B\u044F \u0440\u0430\u0437\
  \u0431\u043E\u0440\u0430 \u0434\u0430\u043D\u043D\u044B\u0445 XML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Как:
Swift предоставляет `XMLParser` и `XMLDocument` для разбора данных XML. Вот фрагмент для разбора простой строки XML:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Don't forget the party on Friday!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Ваш XMLParserDelegate
    parser.parse()
}
```

Вы также можете генерировать XML с помощью `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Пример вывода:

```xml
<note>
  <to>Tove</to>
</note>
```

## Глубокое Погружение
XML, или Расширяемый Язык Разметки, существует с конца 90-х. Он многословен, но читаем для человека, что делает его подходящим для сложных структур данных. Возможности Swift по разбору XML не так развиты, как возможности ElementTree в Python или JAXB в Java, но они выполняют свою задачу для базовых нужд.

Альтернативы, такие как JSON, часто предпочитают в новых системах из-за их меньшего веса и более простых парсеров, но XML по-прежнему занимает видное место во многих корпоративных и устаревших системах.

При работе с XML в Swift, `XMLParser` является парсером на основе потока, что означает, что он последовательно читает документ XML. Для больших файлов XML это эффективно с точки зрения использования памяти. Однако, если вы ищете простоту и ваши данные XML относительно небольшие, использование `XMLDocument` может быть более простым.

## См. также
- [Руководство по разбору XML от Apple](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [Учебник по XML от W3Schools](https://www.w3schools.com/xml/)
