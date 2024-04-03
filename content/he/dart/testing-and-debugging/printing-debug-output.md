---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:21.972261-07:00
description: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\
  \u05E4\u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D1-Dart \u05D4\u05D9\u05D0\
  \ \u05E2\u05DC \u05EA\u05E6\u05D5\u05D2\u05EA \u05DE\u05D9\u05D3\u05E2 \u05DC\u05E7\
  \u05D5\u05E0\u05E1\u05D5\u05DC\u05D4 \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\u05E6\
  \u05D4, \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05E4\u05EA\
  \u05D7\u05D9\u05DD \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05D6\u05E8\
  \u05D9\u05DE\u05EA \u05D4\u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\u05DD, \u05DC\u05D7\
  \u05E7\u05D5\u05E8 \u05D0\u05EA \u05DE\u05E6\u05D1 \u05D4\u05DE\u05E9\u05EA\u05E0\
  \u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D6\u05D4\u05D5\u05EA \u05D0\u05EA \u05DE\u05E7\
  \u05D5\u05E8\u2026"
lastmod: '2024-03-13T22:44:38.850320-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D1-Dart \u05D4\u05D9\u05D0 \u05E2\
  \u05DC \u05EA\u05E6\u05D5\u05D2\u05EA \u05DE\u05D9\u05D3\u05E2 \u05DC\u05E7\u05D5\
  \u05E0\u05E1\u05D5\u05DC\u05D4 \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\u05E6\u05D4\
  , \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05E4\u05EA\u05D7\
  \u05D9\u05DD \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05D6\u05E8\u05D9\
  \u05DE\u05EA \u05D4\u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\u05DD, \u05DC\u05D7\u05E7\
  \u05D5\u05E8 \u05D0\u05EA \u05DE\u05E6\u05D1 \u05D4\u05DE\u05E9\u05EA\u05E0\u05D9\
  \u05DD, \u05D0\u05D5 \u05DC\u05D6\u05D4\u05D5\u05EA \u05D0\u05EA \u05DE\u05E7\u05D5\
  \u05E8 \u05D4\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA."
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 33
---

## איך לעשות:
ב-Dart, אפשר להדפיס פלט לניפוי באגים באמצעות הפונקציה `print()`. הנה איך להדפיס הודעות פשוטות וערכי משתנים:

```dart
void main() {
  String greeting = "שלום, Dart!";
  print(greeting); // מדפיס: שלום, Dart!

  int number = 42;
  print('המספר הוא $number.'); // מדפיס: המספר הוא 42.
}
```

לנתונים מובנים, כמו רשימות או אובייקטים, ייתכן ששיטת ה-`toString()` של Dart לא תספק מספיק פרטים. במקרים כאלה, אפשר להשתמש בפונקציה `jsonEncode` מספריית ה-`dart:convert` של Dart כדי להמיר את הנתונים למחרוזת JSON לפלט קריא יותר:

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // מדפיס: {"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

כאשר נדרשות יכולות ניפוי באגים מתקדמות יותר, כמו תיעוד עם רמות חשיבות שונות (מידע, אזהרה, שגיאה), אפשר להשתמש בספריות של גורמים שלישיים כמו `logger`. הנה איך להשתמש בזה:

1. הוסף את `logger` ל-`pubspec.yaml` שלך:

```yaml
dependencies:
  logger: ^1.0.0
```

2. השתמש ב-`logger` בקוד Dart שלך:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("זו הודעת ניפוי באגים");
  logger.w("זו הודעת אזהרה");
  logger.e("זו הודעת שגיאה");
}
```

הפלט יהיה מודיע יותר, מציג את רמת ההודעה ואת ההודעה עצמה, מה שהופך את ההבחנה בין סוגי הודעות התיעוד לקלה יותר.
