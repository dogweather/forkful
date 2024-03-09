---
title:                "עבודה עם XML"
date:                  2024-03-08T21:57:55.598371-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם XML ב-Dart כוללת פירוש, שאילתא ושינוי מסמכי XML, תהליך קריטי עבור אפליקציות המתקשרות עם שירותי רשת, קובצי תצורה או מערכות ישנות. תכנתים עושים זאת כדי לאפשר החלפת נתונים, תצורות או אפילו קריאות פרוצדורה מרחוק בפורמט מבני, היררכי, שהוא גם קריא לאדם וגם ניתן לניתוח מכונה.

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
