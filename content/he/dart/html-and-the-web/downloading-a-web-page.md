---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:33.522911-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Dart \u05DE\u05E1\
  \u05E4\u05E7\u05EA \u05D0\u05EA \u05D4\u05D7\u05D1\u05D9\u05DC\u05D4 `http`, \u05E1\
  \u05E4\u05E8\u05D9\u05D9\u05D4 \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05E4\
  \u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\u05EA \u05DC\u05D1\u05D9\u05E6\u05D5\u05E2\
  \ \u05D1\u05E7\u05E9\u05D5\u05EA HTTP. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\
  \u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05E9\u05DC \u05D0\u05D9\u05DA \u05DC\
  \u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D5\
  \u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8: \u05E8\
  \u05D0\u05E9\u05D9\u05EA, \u05D4\u05D5\u05E1\u05E3\u2026"
lastmod: '2024-03-13T22:44:38.843539-06:00'
model: gpt-4-0125-preview
summary: "Dart \u05DE\u05E1\u05E4\u05E7\u05EA \u05D0\u05EA \u05D4\u05D7\u05D1\u05D9\
  \u05DC\u05D4 `http`, \u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05E6\u05D3 \u05E9\u05DC\
  \u05D9\u05E9\u05D9 \u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\u05EA \u05DC\u05D1\
  \u05D9\u05E6\u05D5\u05E2 \u05D1\u05E7\u05E9\u05D5\u05EA HTTP."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05E2\u05DE\u05D5\u05D3 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8"
weight: 42
---

## איך לעשות:
Dart מספקת את החבילה `http`, ספרייה צד שלישי פופולרית לביצוע בקשות HTTP. הנה דוגמה בסיסית של איך להשתמש בה כדי להוריד דף אינטרנט:

ראשית, הוסף את חבילת `http` לקובץ `pubspec.yaml` שלך:

```yaml
dependencies:
  http: ^0.13.3
```

לאחר מכן, יבוא את החבילה והשתמש בה כדי לשלוף את תוכן של דף אינטרנט:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('הדף הורד:');
    print(response.body);
  } else {
    print('הבקשה נכשלה עם סטטוס: ${response.statusCode}.');
  }
}
```

**פלט לדוגמה** (זה ישתנה בהתאם לתוכן של הדף האינטרנטי):

```
הדף הורד:
<!doctype html>
<html>
<head>
    <title>דוגמא לדומיין</title>
...
</html>
```

לתרחישים מורכבים יותר, כמו טיפול בעוגיות או הגדרת כותרות משתמש, תשתמש באותה חבילת `http` אבל עם הגדרות נוספות לבקשה שלך:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'YourCustomUserAgent/1.0',
    'Cookie': 'name=value; name2=value2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('הדף הורד עם כותרות מותאמות אישית:');
    print(response.body);
  } else {
    print('הבקשה נכשלה עם סטטוס: ${response.statusCode}.');
  }
}
```

השימוש בכותרות כאלה יכול לחקות בקשות דפדפן בצורה מדויקת יותר, שימושי במיוחד בעת התמודדות עם אתרים שיש להם דרישות מסוימות או הגנות נגד גריפה.
