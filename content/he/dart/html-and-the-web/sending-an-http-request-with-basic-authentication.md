---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-03-08T21:56:39.132188-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
