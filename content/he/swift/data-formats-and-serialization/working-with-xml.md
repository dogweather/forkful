---
date: 2024-01-26 04:36:38.691060-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Swift \u05DE\u05E1\
  \u05E4\u05E7\u05EA \u05D0\u05EA `XMLParser` \u05D5-`XMLDocument` \u05DC\u05E0\u05D9\
  \u05EA\u05D5\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9 XML. \u05D4\u05E0\u05D4 \u05E7\
  \u05D8\u05E2 \u05E7\u05D5\u05D3 \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA XML \u05E4\u05E9\u05D5\u05D8\u05D4."
lastmod: '2024-03-13T22:44:39.945759-06:00'
model: gpt-4-0125-preview
summary: "Swift \u05DE\u05E1\u05E4\u05E7\u05EA \u05D0\u05EA `XMLParser` \u05D5-`XMLDocument`\
  \ \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9 XML."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## איך לעשות:
Swift מספקת את `XMLParser` ו-`XMLDocument` לניתוח נתוני XML. הנה קטע קוד לניתוח מחרוזת XML פשוטה:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>אל תשכח את המסיבה ביום שישי!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // נציג XMLParserDelegate שלך
    parser.parse()
}
```

ניתן גם לייצר XML באמצעות `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

פלט לדוגמא:

```xml
<note>
  <to>Tove</to>
</note>
```

## צלילה עמוקה
XML, או שפת סימון להרחבה, היא קיימת מאז שנות ה-90 המאוחרות. היא מפורטת אך קריאה לאדם, מה שהופך אותה למתאימה למבני נתונים מורכבים. יכולות הניתוח של Swift ל-XML אינן חזקות כמו אלו שנמצאות ב-ElementTree של Python או ב-JAXB של Java, אך הן מספקות פתרון לצרכים בסיסיים.

אלטרנטיבות כמו JSON מועדפות לעיתים קרובות במערכות חדשות בזכות משקלן הקל יותר ומנתחים פחות מורכבים, אך XML עדיין נמצאת בשימוש רב במערכות ארגוניות ויישומים קיימים.

כאשר עובדים עם XML ב-Swift, `XMLParser` הוא מנתח מבוסס זרם, מה שאומר שהוא קורא את המסמך XML בצורה רציפה. לקבצי XML גדולים, זה יעיל מבחינת זיכרון. עם זאת, אם אתה מחפש פשטות והנתונים שלך ב-XML הם בגודל סביר, שימוש ב-`XMLDocument` עשוי להיות יותר פשוט.

## ראה גם
- [מדריך ניתוח XML של Apple](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [מדריך XML של W3Schools](https://www.w3schools.com/xml/)
