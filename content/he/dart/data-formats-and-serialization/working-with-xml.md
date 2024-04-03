---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:55.598371-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1-Dart \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9, \u05E9\u05D0\u05D9\u05DC\u05EA\
  \u05D0 \u05D5\u05E9\u05D9\u05E0\u05D5\u05D9 \u05DE\u05E1\u05DE\u05DB\u05D9 XML,\
  \ \u05EA\u05D4\u05DC\u05D9\u05DA \u05E7\u05E8\u05D9\u05D8\u05D9 \u05E2\u05D1\u05D5\
  \u05E8 \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D5\u05EA \u05D4\u05DE\u05EA\
  \u05E7\u05E9\u05E8\u05D5\u05EA \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\
  \ \u05E8\u05E9\u05EA, \u05E7\u05D5\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4\
  \ \u05D0\u05D5 \u05DE\u05E2\u05E8\u05DB\u05D5\u05EA \u05D9\u05E9\u05E0\u05D5\u05EA\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA\u2026"
lastmod: '2024-03-13T22:44:38.887741-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1-Dart \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9, \u05E9\u05D0\u05D9\u05DC\u05EA\
  \u05D0 \u05D5\u05E9\u05D9\u05E0\u05D5\u05D9 \u05DE\u05E1\u05DE\u05DB\u05D9 XML,\
  \ \u05EA\u05D4\u05DC\u05D9\u05DA \u05E7\u05E8\u05D9\u05D8\u05D9 \u05E2\u05D1\u05D5\
  \u05E8 \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D5\u05EA \u05D4\u05DE\u05EA\
  \u05E7\u05E9\u05E8\u05D5\u05EA \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\
  \ \u05E8\u05E9\u05EA, \u05E7\u05D5\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4\
  \ \u05D0\u05D5 \u05DE\u05E2\u05E8\u05DB\u05D5\u05EA \u05D9\u05E9\u05E0\u05D5\u05EA\
  ."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## איך ל:
Dart אינו כולל תמיכה מובנית לטיפול ב-XML בספרייתו הסטנדרטית, מה שמחייב שימוש בחבילות צד שלישי. חבילה פופולרית היא `xml`. לשימוש בה, תחילה עליך להוסיף אותה ל-`pubspec.yaml` שלך:

```yaml
dependencies:
  xml: ^5.0.0 // השתמשו בגרסה האחרונה הזמינה
```

לאחר מכן, יבאו את החבילה בקובץ Dart שלכם:

```dart
import 'package:xml/xml.dart' as xml;
```

**פירוש XML:**

נניח שיש לכם מחרוזת XML כזו:

```xml
<String name="greeting">שלום, עולם!</String>
```

ניתן לפרש ולקרוא את ה-XML כמו זה:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // מוצג: greeting
}

void main() {
  final xmlString = '<String name="greeting">שלום, עולם!</String>';
  parseXml(xmlString);
}
```

**יצירת מסמכי XML:**

יצירת מסמך XML חדש היא פשוטה עם החבילה `xml`:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('שלום, עולם!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**פלט**:

```xml
<?xml version="1.0"?>
<greeting name="hello">שלום, עולם!</greeting>
```

**שאילתא ושינוי XML:**

לחיפוש או שינוי אלמנטים, ניתן להשתמש בשיטות דומות ל-XPath:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // שינוי התכונה 'name'
    greeting.setAttribute('name', 'greeting_modified');
    
    // הוספת אלמנט ילד חדש
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('להתראות!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">שלום, עולם!</greeting>';
  modifyXml(xmlString);
}
```

**פלט**:

```xml
<greeting name="greeting_modified">
  שלום, עולם!
  <message>להתראות!</message>
</greeting>
```

דוגמאות אלה מדגימות פעולות בסיסיות לעבודה עם XML ב-Dart. עם החבילה `xml`, תוכלו לפרש, ליצור ולשנות מסמכי XML לפי דרישות האפליקציה שלכם.
