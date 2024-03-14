---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:39.132188-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05E6\u05D9\u05E8\u05D5\u05E3 \u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\
  \u05E9 \u05D5\u05E1\u05D9\u05E1\u05DE\u05D4 \u05DC\u05D1\u05E7\u05E9\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05D0\u05DE\u05EA \u05D0\u05EA \u05D6\u05D4\u05D5\u05EA \u05D4\
  \u05DE\u05E9\u05EA\u05DE\u05E9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\
  \u05D2\u05E9\u05EA \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9\u05DD \u05E9\u05D3\u05D5\
  \u05E8\u05E9\u05D9\u05DD \u05D0\u05D9\u05DE\u05D5\u05EA, \u05DE\u05D4 \u05E9\u05DE\
  \u05D1\u05D8\u05D9\u05D7\u2026"
lastmod: '2024-03-13T22:44:38.845307-06:00'
model: gpt-4-0125-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05E6\u05D9\u05E8\u05D5\u05E3 \u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\
  \u05E9 \u05D5\u05E1\u05D9\u05E1\u05DE\u05D4 \u05DC\u05D1\u05E7\u05E9\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05D0\u05DE\u05EA \u05D0\u05EA \u05D6\u05D4\u05D5\u05EA \u05D4\
  \u05DE\u05E9\u05EA\u05DE\u05E9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\
  \u05D2\u05E9\u05EA \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9\u05DD \u05E9\u05D3\u05D5\
  \u05E8\u05E9\u05D9\u05DD \u05D0\u05D9\u05DE\u05D5\u05EA, \u05DE\u05D4 \u05E9\u05DE\
  \u05D1\u05D8\u05D9\u05D7\u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי כוללת צירוף שם משתמש וסיסמה לבקשה כדי לאמת את זהות המשתמש. מתכנתים משתמשים בזה כדי לגשת למשאבים שדורשים אימות, מה שמבטיח תקשורת מאובטחת בין הלקוח לשרת.

## איך לעשות:

ב-Dart, ניתן להשתמש בחבילת `http` כדי לשלוח בקשות HTTP עם אימות בסיסי. ראשית, הוסף את חבילת `http` לקובץ `pubspec.yaml` שלך:

```yaml
dependencies:
  http: ^0.13.4
```

לאחר מכן, ייבא את החבילה בקובץ Dart שלך:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

כדי לשלוח בקשת GET עם אימות בסיסי, ניתן להשתמש בקוד הבא:

```dart
Future<void> fetchUserData() async {
  final username = 'yourUsername';
  final password = 'yourPassword';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://yourapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('נתוני המשתמש נאספו בהצלחה!');
    print('גוף התגובה: ${response.body}');
  } else {
    print('נכשל באיסוף נתוני המשתמש עם קוד הסטטוס: ${response.statusCode}');
  }
}
```

קוד זה שולח בקשת GET ל'https://yourapi.com/userdata' עם כותרת אימות בסיסית. שם המשתמש והסיסמה מקודדים ב-base64 ומועברים בכותרת 'Authorization' על פי תקנות האימות הבסיסיות.

**פלט לדוגמא:**

לאחר בקשה מוצלחת ואם השרת מחזיר קוד סטטוס של 200, ייתכן ותראה:

```plaintext
נתוני המשתמש נאספו בהצלחה!
גוף התגובה: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

אם האימות נכשל או שיש שגיאה אחרת, קוד סטטוס התגובה יעזור לזהות את הבעיה.
