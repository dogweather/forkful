---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:28.884863-07:00
description: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\
  \u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05EA \u05E9\
  \u05D0\u05D9\u05DC\u05EA\u05EA \u05D4\u05DE\u05E2\u05E8\u05DB\u05EA \u05E2\u05D1\
  \u05D5\u05E8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05D4\u05E9\u05E2\u05D4\
  \ \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9\u05D9\u05DD. \u05D4\u05E4\u05D5\u05E0\u05E7\
  \u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D4\u05D6\u05D5 \u05E9\u05D9\
  \u05DE\u05D5\u05E9\u05D9\u05EA \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3 \u05D1\u05D0\
  \u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D5\u05EA \u05DC\u05EA\u05DB\u05D5\u05E0\u05D5\
  \u05EA \u05DB\u05DE\u05D5 \u05D4\u05E6\u05D1\u05EA \u05D7\u05D5\u05EA\u05DE\u05EA\
  \ \u05D6\u05DE\u05DF \u05DC\u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD,\u2026"
lastmod: '2024-03-11T00:14:12.292643-06:00'
model: gpt-4-0125-preview
summary: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9 \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05EA \u05E9\u05D0\
  \u05D9\u05DC\u05EA\u05EA \u05D4\u05DE\u05E2\u05E8\u05DB\u05EA \u05E2\u05D1\u05D5\
  \u05E8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05D4\u05E9\u05E2\u05D4 \u05D4\
  \u05E0\u05D5\u05DB\u05D7\u05D9\u05D9\u05DD. \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D4\u05D6\u05D5 \u05E9\u05D9\u05DE\
  \u05D5\u05E9\u05D9\u05EA \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3 \u05D1\u05D0\u05E4\
  \u05DC\u05D9\u05E7\u05E6\u05D9\u05D5\u05EA \u05DC\u05EA\u05DB\u05D5\u05E0\u05D5\u05EA\
  \ \u05DB\u05DE\u05D5 \u05D4\u05E6\u05D1\u05EA \u05D7\u05D5\u05EA\u05DE\u05EA \u05D6\
  \u05DE\u05DF \u05DC\u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD,\u2026"
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי ב-Dart כוללת שאילתת המערכת עבור התאריך והשעה הנוכחיים. הפונקציונליות הזו שימושית במיוחד באפליקציות לתכונות כמו הצבת חותמת זמן לאירועים, הצגת התאריך הנוכחי למשתמשים, או חישוב משכים. לדעת כיצד לאחזר ולנהל את התאריך הנוכחי בצורה יעילה היא בסיס לתזמון, רישום, ותכונות התלויות בזמן.

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
