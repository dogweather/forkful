---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:19.625643-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Dart, \u05D0\
  \u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05DE\u05D9\u05E8 \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\
  \u05E0\u05D5\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E9\u05D9\
  \u05D8\u05D4 `toLowerCase()` \u05E9\u05E0\u05D9\u05EA\u05E0\u05EA \u05E2\u05DC \u05D9\
  \u05D3\u05D9 \u05DE\u05D7\u05DC\u05E7\u05EA `String`. \u05E9\u05D9\u05D8\u05D4 \u05D6\
  \u05D5 \u05DE\u05D7\u05D6\u05D9\u05E8\u05D4 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05D7\u05D3\u05E9\u05D4 \u05E2\u05DD \u05DB\u05DC \u05D4\u05EA\u05D5\u05D5\u05D9\
  \u05DD\u2026"
lastmod: '2024-03-13T22:44:38.822818-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Dart, \u05D0\u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05DE\
  \u05D9\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\
  \u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\
  \u05EA \u05D4\u05E9\u05D9\u05D8\u05D4 `toLowerCase()` \u05E9\u05E0\u05D9\u05EA\u05E0\
  \u05EA \u05E2\u05DC \u05D9\u05D3\u05D9 \u05DE\u05D7\u05DC\u05E7\u05EA `String`."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

## איך לעשות:
ב-Dart, אתה יכול להמיר מחרוזת לאותיות קטנות באמצעות השיטה `toLowerCase()` שניתנת על ידי מחלקת `String`. שיטה זו מחזירה מחרוזת חדשה עם כל התווים הגדולים המומרים לאותיות קטנות. בואו נראה איך זה עובד עם דוגמה פשוטה:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // פלט: hello, world!
}
```

Dart אינו דורש ספריות חיצוניות למשימות טיפול במחרוזות בסיסיות, כולל המרה לאותיות קטנות, כיוון שמחלקת `String` של הספרייה הסטנדרטית היא מקיפה מאוד. עם זאת, למניפולציות מורכבות יותר הכוללות כללים ספציפיים לאזור, כדאי לשקול את החבילה `intl`, שמספקת יכולות בינלאומיות ולוקליזציה, כולל המרה של רישיות מבוססת אזור:

כדי להשתמש ב-`intl`, הוסף אותו לקובץ `pubspec.yaml` שלך:

```yaml
dependencies:
  intl: ^0.17.0
```

לאחר מכן, אתה יכול להשתמש בשיטה `toLocaleLowerCase()` כדי להמיר מחרוזת לאותיות קטנות בהתבסס על אזורים ספציפיים:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // אזור טורקי
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // פלט: istanbul
  
  // אזור ברירת המחדל (en)
  print(originalString.toLowerCase()); // פלט: i̇stanbul
}
```

בדוגמה זו, שימו לב איך האזור הטורקי מטפל בצורה נכונה ב-'i' ללא נקודה, מה שמדגים את החשיבות של המרות מודעות לאזור באפליקציות בינלאומיות.
