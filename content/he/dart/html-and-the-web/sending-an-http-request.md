---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:08.106737-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D1\
  -Dart \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05D9\
  \u05E6\u05D9\u05E8\u05EA \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA \u05E2\u05DD \u05E9\
  \u05E8\u05EA \u05D5\u05D5\u05D1 \u05D0\u05D5 API \u05DE\u05D9\u05D9\u05E9\u05D5\u05DD\
  \ Dart. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D7\u05D6\u05E8 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05DE\u05D4\u05E8\u05E9\u05EA, \u05DC\u05E9\u05DC\u05D5\
  \u05D7 \u05D8\u05E4\u05E1\u05D9\u05DD, \u05D5\u05DC\u05D4\u05EA\u05DE\u05DE\u05E9\
  \u05E7 \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\u2026"
lastmod: '2024-03-09T21:06:03.623825-07:00'
model: gpt-4-0125-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D1-Dart\
  \ \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05D9\u05E6\
  \u05D9\u05E8\u05EA \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA \u05E2\u05DD \u05E9\u05E8\
  \u05EA \u05D5\u05D5\u05D1 \u05D0\u05D5 API \u05DE\u05D9\u05D9\u05E9\u05D5\u05DD\
  \ Dart. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D7\u05D6\u05E8 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05DE\u05D4\u05E8\u05E9\u05EA, \u05DC\u05E9\u05DC\u05D5\
  \u05D7 \u05D8\u05E4\u05E1\u05D9\u05DD, \u05D5\u05DC\u05D4\u05EA\u05DE\u05DE\u05E9\
  \u05E7 \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP ב-Dart היא התהליך של יצירת תקשורת עם שרת ווב או API מיישום Dart. מתכנתים עושים זאת כדי לאחזר נתונים מהרשת, לשלוח טפסים, ולהתממשק עם שירותי REST, דבר ההופך את זה לפעולה יסודית לפיתוח אפליקציות וב, שרת צידיות, ופיתוח אפליקציות ניידות ב-Dart.

## איך לעשות:

Dart כוללת את החבילה `http`, דרך חזקה ונוחה לעבוד עם משאבי HTTP. תחילה, כלול אותה בקובץ pubspec.yaml שלך:

```yaml
dependencies:
  http: ^0.13.3
```

לאחר מכן, ייבא אותה בקוד Dart שלך כדי להתחיל לשלוח בקשות:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('גוף התגובה: ${response.body}');
  } else {
    print('הבקשה נכשלה עם סטטוס: ${response.statusCode}.');
  }
}
```

הפלט לבקשה מוצלחת עשוי להיראות כך:

```
גוף התגובה: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

לבקשות מורכבות יותר, כמו בקשות POST עם גוף JSON, היית עושה כדלקמן:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var response = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (response.statusCode == 201) {
    print('סטטוס התגובה: ${response.statusCode}');
    print('גוף התגובה: ${response.body}');
  } else {
    print('נכשל ביצירת פוסט חדש. סטטוס: ${response.statusCode}');
  }
}
```

הפלט לבקשת ה-POST עשוי להיראות:

```
סטטוס התגובה: 201
גוף התגובה: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

דוגמאות אלו מציגות בקשות HTTP GET ו-POST בסיסיות באמצעות החבילה `http` ב-Dart. חבילה זו מכסה את רוב הצרכים לשליחת בקשות HTTP, כולל תרחישים מורכבים יותר עם כותרות ותוכן גוף.
