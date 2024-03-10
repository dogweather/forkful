---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:21.553675-07:00
description: "\u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Dart \u05D4\u05D9\u05D0 \u05DE\
  \u05E9\u05D9\u05DE\u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05DB\u05D0\u05E9\u05E8\
  \ \u05E6\u05E8\u05D9\u05DA \u05DC\u05D4\u05E6\u05D9\u05D2 \u05DE\u05D9\u05D3\u05E2\
  \ \u05E2\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\u05D4 \u05D1\u05E4\
  \u05D5\u05E8\u05DE\u05D8 \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\u05D3\u05DD, \u05D0\
  \u05D5 \u05DB\u05D0\u05E9\u05E8 \u05D0\u05EA\u05D4 \u05DE\u05EA\u05DB\u05D5\u05D5\
  \u05DF \u05DC\u05E9\u05E8\u05E9\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\
  \u05D0\u05D7\u05E1\u05D5\u05DF \u05D0\u05D5 \u05E9\u05D9\u05D3\u05D5\u05E8. \u05EA\
  \u05D4\u05DC\u05D9\u05DA\u2026"
lastmod: '2024-03-09T21:06:03.639601-07:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Dart \u05D4\u05D9\u05D0 \u05DE\u05E9\
  \u05D9\u05DE\u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05DB\u05D0\u05E9\u05E8 \u05E6\
  \u05E8\u05D9\u05DA \u05DC\u05D4\u05E6\u05D9\u05D2 \u05DE\u05D9\u05D3\u05E2 \u05E2\
  \u05DC \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\u05D4 \u05D1\u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\u05D3\u05DD, \u05D0\u05D5\
  \ \u05DB\u05D0\u05E9\u05E8 \u05D0\u05EA\u05D4 \u05DE\u05EA\u05DB\u05D5\u05D5\u05DF\
  \ \u05DC\u05E9\u05E8\u05E9\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\u05D0\
  \u05D7\u05E1\u05D5\u05DF \u05D0\u05D5 \u05E9\u05D9\u05D3\u05D5\u05E8. \u05EA\u05D4\
  \u05DC\u05D9\u05DA\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

המרה של תאריך למחרוזת ב-Dart היא משימה נפוצה כאשר צריך להציג מידע על תאריך ושעה בפורמט קריא לאדם, או כאשר אתה מתכוון לשרשר נתונים לאחסון או שידור. תהליך זה מאפשר הצגה ותפעול קלים של ערכי תאריך-שעה בפורמט שהוא מובן וניתן להתאמה תלוי במקרה השימוש.

## איך לעשות:

Dart מספקת את הכיתה `DateTime` לטיפול בתאריכים ושעות, ואת החבילה `intl` לעיצוב. ראשית, ודא שיש לך את החבילה `intl` על ידי הוספת `intl: ^0.17.0` (או הגרסה האחרונה) לקובץ `pubspec.yaml` שלך.

### שימוש בספריית הליבה של Dart

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // פלט: 2023-4-12 (לדוגמה, זה תלוי בתאריך הנוכחי)
```

דוגמה זו בונה ישירות מחרוזת מהתכונות של `DateTime`.

### שימוש בחבילת `intl`

ראשית, ייבא את החבילה:

```dart
import 'package:intl/intl.dart';
```

לאחר מכן, עיצב את התאריך:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // פלט: 2023-04-12
```

החבילה `intl` מאפשרת עיצוב מורכב הרבה יותר בקלות, כולל פורמטים ספציפיים לאזור:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // פלט: April 12, 2023
```

דוגמאות אלה מראות דרכים פשוטות אך עוצמתיות להמרה ולעיצוב של תאריכים למחרוזות ב-Dart, בין אם על ידי שימוש ביכולות הליבה של Dart או בהתקנת החבילה `intl` לאפשרויות עיצוב מתקדמות יותר.
