---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:09.270569-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05EA \u05D4\u05DC\u05D9\u05D1\u05D4 \u05E9\u05DC Dart \u05DE\u05E4\
  \u05E9\u05D8\u05EA \u05D0\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05D4\u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D3\u05E8\u05DA \u05D4\u05DE\u05D7\u05DC\u05E7\
  \u05D4 `DateTime`. \u05DC\u05DE\u05E7\u05E8\u05D9\u05DD \u05E4\u05E9\u05D5\u05D8\
  \u05D9\u05DD \u05E9\u05D1\u05D4\u05DD \u05D0\u05EA\u05D4 \u05D9\u05D5\u05D3\u05E2\
  \ \u05D0\u05EA \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05E9\u05DC \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA, \u05EA\u05D5\u05DB\u05DC\
  \ \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E9\u05D9\u05D8\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.862402-06:00'
model: gpt-4-0125-preview
summary: "\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05DC\u05D9\u05D1\u05D4 \u05E9\
  \u05DC Dart \u05DE\u05E4\u05E9\u05D8\u05EA \u05D0\u05EA \u05E0\u05D9\u05EA\u05D5\
  \u05D7 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D3\u05E8\u05DA \u05D4\
  \u05DE\u05D7\u05DC\u05E7\u05D4 `DateTime`."
title: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## איך לעשות:
ספריית הליבה של Dart מפשטת את ניתוח התאריכים דרך המחלקה `DateTime`. למקרים פשוטים שבהם אתה יודע את פורמט המחרוזת של התאריך, תוכל להשתמש בשיטת `DateTime.parse()`. עם זאת, לסיטואציות מורכבות יותר או כשמתמודדים עם מספר פורמטים, החבילה `intl`, ובפרט המחלקה `DateFormat`, הופכת לבלתי נמנעת.

### שימוש בספריית הליבה של Dart:
```dart
void main() {
  // שימוש ב-DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### שימוש בחבילת `intl`:
ראשית, הוסף את חבילת `intl` לקובץ `pubspec.yaml` שלך:
```yaml
dependencies:
  intl: ^0.17.0
```
לאחר מכן, ייבא את החבילה והשתמש ב-`DateFormat` לניתוח:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
החבילה `intl` מציעה אפשרויות חזקות לניתוח תאריכים, ומאפשרת התמודדות חלקה עם מגוון רחב של פורמטים בינלאומיים.
