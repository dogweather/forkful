---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:00.561648-07:00
description: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D1-Dart \u05DB\u05E8\u05D5\u05DB\u05D4 \u05D1\
  \u05D4\u05E2\u05E8\u05DB\u05EA \u05D4\u05D4\u05D1\u05D3\u05DC \u05D0\u05D5 \u05D4\
  \u05E1\u05D3\u05E8 \u05D4\u05D6\u05DE\u05E0\u05D9 \u05D1\u05D9\u05E0\u05D9\u05D4\
  \u05DD, \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA\
  \ \u05D7\u05D9\u05D5\u05E0\u05D9\u05EA \u05D1\u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\
  \u05DD \u05D4\u05DE\u05E0\u05D4\u05DC\u05D9\u05DD \u05D0\u05D9\u05E8\u05D5\u05E2\
  \u05D9\u05DD, \u05D3\u05D3\u05DC\u05D9\u05D9\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DB\
  \u05DC \u05E0\u05EA\u05D5\u05DF \u05E8\u05D2\u05D9\u05E9 \u05DC\u05D6\u05DE\u05DF\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.867550-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05D1-Dart \u05DB\u05E8\u05D5\u05DB\u05D4 \u05D1\u05D4\
  \u05E2\u05E8\u05DB\u05EA \u05D4\u05D4\u05D1\u05D3\u05DC \u05D0\u05D5 \u05D4\u05E1\
  \u05D3\u05E8 \u05D4\u05D6\u05DE\u05E0\u05D9 \u05D1\u05D9\u05E0\u05D9\u05D4\u05DD\
  , \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D7\
  \u05D9\u05D5\u05E0\u05D9\u05EA \u05D1\u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD\
  \ \u05D4\u05DE\u05E0\u05D4\u05DC\u05D9\u05DD \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\
  \u05DD, \u05D3\u05D3\u05DC\u05D9\u05D9\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DB\u05DC\
  \ \u05E0\u05EA\u05D5\u05DF \u05E8\u05D2\u05D9\u05E9 \u05DC\u05D6\u05DE\u05DF."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05E0\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

## איך לעשות זאת:
ב-Dart, ניתן להשוות תאריכים באמצעות מחלקת `DateTime`, המציעה שיטות כמו `isBefore`, `isAfter`, ו-`isAtSameMomentAs` להשוואה ישירה. נוסף על כך, ניתן לקבוע את ההבדל בין תאריכים באמצעות השיטה `difference()`, המספקת אובייקט מסוג `Duration` המפרט את הפער בין שני נקודות בזמן.

הנה דוגמה בסיסית הממחישה את המושגים הללו:

```dart
void main() {
  DateTime eventStart = DateTime(2023, 5, 15);
  DateTime eventEnd = DateTime(2023, 5, 20);
  
  // בדיקה אם תאריך אחד לפני השני
  if (eventStart.isBefore(eventEnd)) {
    print("תאריך תחילת האירוע לפני תאריך סיום האירוע.");
  }

  // בדיקה אם שני התאריכים זהים
  if (!eventStart.isAtSameMomentAs(eventEnd)) {
    print("תאריכי התחלה וסיום האירוע אינם זהים.");
  }
  
  // חישוב ההבדל בין שני תאריכים
  Duration eventDuration = eventEnd.difference(eventStart);
  print("משך האירוע הוא ${eventDuration.inDays} ימים.");
}

/*
פלט:
תאריך תחילת האירוע לפני תאריך סיום האירוע.
תאריכי התחלה וסיום האירוע אינם זהים.
משך האירוע הוא 5 ימים.
*/
```

למניפולציות תאריכים מתקדמות יותר, כמו המרות פורמטים, ייתכן ותמצאו את מחלקת `DateFormat` מהחבילה `intl` מועילה. להלן דוגמה המדגימה איך להשתמש בה לצורך פורמט והשוואת תאריכים:

ראשית, כלול את החבילה `intl` בקובץ `pubspec.yaml` שלך:

```yaml
dependencies:
  intl: ^0.17.0
```

לאחר מכן, השתמש בה כך:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime departureDate = DateTime(2023, 5, 15);
  DateTime returnDate = DateTime.parse('2023-05-20');

  // פורמט תאריכים
  var formatter = DateFormat('yyyy-MM-dd');
  print("יציאה: ${formatter.format(departureDate)}");
  print("חזרה: ${formatter.format(returnDate)}");

  // השוואה באמצעות מחרוזות מפורמטות
  if (formatter.format(departureDate) == formatter.format(returnDate)) {
    print("תאריכי היציאה והחזרה זהים.");
  } else {
    print("תאריכי היציאה והחזרה שונים.");
  }
}

/*
פלט:
יציאה: 2023-05-15
חזרה: 2023-05-20
תאריכי היציאה והחזרה שונים.
*/
```

דוגמה זו מציגה כיצד להשוות בין שני אובייקטים מסוג `DateTime` גם באופן ישיר וגם באמצעות מחרוזות מפורמטות להשוואות שצריכות להתעלם מרכיבים ספציפיים כמו הזמן.
