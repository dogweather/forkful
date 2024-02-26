---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:13.441849-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML \u0432 Swift \u043E\u0437\
  \u043D\u0430\u0447\u0430\u0435\u0442 \u0430\u043D\u0430\u043B\u0438\u0437 \u0438\
  \ \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0434\u0430\u043D\u043D\
  \u044B\u0445 XML. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F\
  \ \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\u0438\
  , \u043E\u0441\u043E\u0431\u0435\u043D\u043D\u043E \u043A\u043E\u0433\u0434\u0430\
  \ \u0438\u043D\u0442\u0435\u0433\u0440\u0438\u0440\u0443\u044E\u0442\u0441\u044F\
  \ \u0441 \u0441\u0438\u0441\u0442\u0435\u043C\u0430\u043C\u0438, \u0433\u0434\u0435\
  \ XML\u2026"
lastmod: '2024-02-25T18:49:43.388590-07:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML \u0432 Swift \u043E\u0437\
  \u043D\u0430\u0447\u0430\u0435\u0442 \u0430\u043D\u0430\u043B\u0438\u0437 \u0438\
  \ \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0434\u0430\u043D\u043D\
  \u044B\u0445 XML. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F\
  \ \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\u0438\
  , \u043E\u0441\u043E\u0431\u0435\u043D\u043D\u043E \u043A\u043E\u0433\u0434\u0430\
  \ \u0438\u043D\u0442\u0435\u0433\u0440\u0438\u0440\u0443\u044E\u0442\u0441\u044F\
  \ \u0441 \u0441\u0438\u0441\u0442\u0435\u043C\u0430\u043C\u0438, \u0433\u0434\u0435\
  \ XML\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
---

{{< edit_this_page >}}

## Что и Почему?
Работа с XML в Swift означает анализ и генерацию данных XML. Программисты делают это для обмена данными, особенно когда интегрируются с системами, где XML является стандартным форматом.

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
