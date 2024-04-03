---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:19.239670-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Dart \u05DE\u05E1\
  \u05E4\u05E7\u05EA \u05EA\u05DE\u05D9\u05DB\u05D4 \u05D7\u05D6\u05E7\u05D4 \u05DC\
  \u05E0\u05D9\u05D4\u05D5\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D3\
  \u05E8\u05DA \u05D4\u05DE\u05D7\u05DC\u05E7\u05D4 `DateTime`. \u05D4\u05E0\u05D4\
  \ \u05D0\u05D9\u05DA \u05D0\u05EA\u05DD \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\
  \u05D7\u05E9\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05E2\u05EA\
  \u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D1\u05D0\u05DE\u05E6\u05E2\
  \u05D5\u05EA Dart \u05D8\u05D1\u05E2\u05D9, \u05DC\u05DC\u05D0 \u05E6\u05D5\u05E8\
  \u05DA \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.869324-06:00'
model: gpt-4-0125-preview
summary: "Dart \u05DE\u05E1\u05E4\u05E7\u05EA \u05EA\u05DE\u05D9\u05DB\u05D4 \u05D7\
  \u05D6\u05E7\u05D4 \u05DC\u05E0\u05D9\u05D4\u05D5\u05DC \u05EA\u05D0\u05E8\u05D9\
  \u05DB\u05D9\u05DD \u05D3\u05E8\u05DA \u05D4\u05DE\u05D7\u05DC\u05E7\u05D4 `DateTime`."
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

## איך לעשות:
Dart מספקת תמיכה חזקה לניהול תאריכים דרך המחלקה `DateTime`. הנה איך אתם יכולים לחשב תאריכים בעתיד או בעבר באמצעות Dart טבעי, ללא צורך בספריות צד שלישי.

### חישוב תאריך עתידי
כדי לחשב תאריך בעתיד, יוצרים אובייקט `DateTime` ומשתמשים בשיטה `add` עם המשך הזמן הרצוי.

```dart
DateTime today = DateTime.now();
Duration tenDays = Duration(days: 10);
DateTime futureDate = today.add(tenDays);

print(futureDate); // פלט: 2023-04-21 14:22:35.123456 (פלט לדוגמה, תלוי בתאריך ושעה הנוכחיים)
```

### חישוב תאריך עבר
לחישוב תאריך בעבר, משתמשים בשיטה `subtract` על אובייקט `DateTime` עם המשך הזמן הנדרש.

```dart
DateTime today = DateTime.now();
Duration fifteenDaysAgo = Duration(days: 15);
DateTime pastDate = today.subtract(fifteenDaysAgo);

print(pastDate); // פלט: 2023-03-27 14:22:35.123456 (פלט לדוגמה, תלוי בתאריך ושעה הנוכחיים)
```

### שימוש בספריות צד שלישי
למרות שהיכולות הטבעיות של Dart לניהול תאריכים הן עצמתיות, ייתכן שתמצאו את עצמכם זקוקים לפעולות ספציפיות יותר, כמו פרסור או עיצוב תאריכים בקלות רבה יותר, או ביצוע חישובים מורכבים. במקרים כאלו, החבילה `time` יכולה להיות שימושית מאוד.

תחילה, הוסף את `time` לתלותיות שלך ב-`pubspec.yaml`:

```yaml
dependencies:
  time: ^2.0.0
```

לאחר מכן, אתה יכול להשתמש בה לביצוע חישובים דומים עם קריאות טובה יותר:

```dart
import 'package:time/time.dart';

void main() {
  DateTime today = DateTime.now();

  // חישוב תאריך עתידי
  DateTime futureDate = today + 10.days;
  print(futureDate); // פורמט פלט: 2023-04-21 14:22:35.123456

  // חישוב תאריך עבר
  DateTime pastDate = today - 15.days;
  print(pastDate); // פורמט פלט: 2023-03-27 14:22:35.123456
}
```

דוגמאות אלו ממחישות ניהול תאריכים בסיסי ב-Dart, כולל הוספה והפחתה של זמן לתאריך נוכחי או ממנו, מדגימות איך בקלות ניתן לנהל תאריכים באפליקציות Dart.
