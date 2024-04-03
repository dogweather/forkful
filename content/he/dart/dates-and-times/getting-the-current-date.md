---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:28.884863-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05DC\u05D9\u05D1\u05D4 \u05E9\u05DC\
  \ Dart \u05DE\u05E1\u05E4\u05E7\u05EA \u05D2\u05D9\u05E9\u05D4 \u05D9\u05E9\u05D9\
  \u05E8\u05D4 \u05DC\u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05DC\u05E9\u05E2\u05D4\
  \ \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9\u05D9\u05DD \u05D3\u05E8\u05DA \u05DE\u05D7\
  \u05DC\u05E7\u05EA `DateTime`. \u05DC\u05D4\u05DC\u05DF \u05D3\u05D5\u05D2\u05DE\
  \u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05DC\u05E7\u05D1\u05DC\u05EA \u05D4\
  \u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9."
lastmod: '2024-03-13T22:44:38.864121-06:00'
model: gpt-4-0125-preview
summary: "\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05DC\u05D9\u05D1\u05D4 \u05E9\
  \u05DC Dart \u05DE\u05E1\u05E4\u05E7\u05EA \u05D2\u05D9\u05E9\u05D4 \u05D9\u05E9\
  \u05D9\u05E8\u05D4 \u05DC\u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05DC\u05E9\u05E2\
  \u05D4 \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9\u05D9\u05DD \u05D3\u05E8\u05DA \u05DE\
  \u05D7\u05DC\u05E7\u05EA `DateTime`."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

## איך לעשות זאת:
ספריית הליבה של Dart מספקת גישה ישירה לתאריך ולשעה הנוכחיים דרך מחלקת `DateTime`. להלן דוגמה בסיסית לקבלת התאריך הנוכחי:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // דוגמת פלט: 2023-04-12 10:00:00.000
}
```

אם אתם צריכים רק את חלק התאריך (שנה, חודש, יום), אתם יכולים לעצב את אובייקט `DateTime`:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // דוגמת פלט: 2023-04-12
}
```

Dart אינה כוללת ספרייה מובנית לעיצוב תאריכים מורכב יותר, אולם אתם יכולים להשתמש בחבילת `intl` למטרה זו. תחילה, הוסיפו את החבילה לקובץ `pubspec.yaml` שלכם:

```yaml
dependencies:
  intl: ^0.17.0
```

לאחר מכן, תוכלו לעצב תאריכים בקלות:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // דוגמת פלט: 2023-04-12
}
```

לאפשרויות עיצוב מתקדמות יותר, חקרו את מחלקת `DateFormat` שמספקת חבילת `intl`, התומכת במגוון רחב של דפוסים ואזורים.
