---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:33.728104-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC \u05E9\u05D2\u05D9\u05D0\
  \u05EA \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8 (stderr) \u05D1-Dart \u05D6\u05D5 \u05E2\
  \u05DC \u05E9\u05DC\u05D9\u05D7\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\
  \u05D2\u05D9\u05D0\u05D4 \u05D5\u05D0\u05D1\u05D7\u05D5\u05E0\u05D9\u05DD \u05DC\
  \u05D6\u05E8\u05DD \u05E0\u05E4\u05E8\u05D3, \u05D1\u05E0\u05E4\u05E8\u05D3 \u05DE\
  \u05D4\u05E4\u05DC\u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (stdout).\
  \ \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D3\u05D9\u05DC \u05D1\u05D9\u05DF\
  \ \u05E4\u05DC\u05D8\u2026"
lastmod: '2024-03-09T21:06:03.644817-07:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC \u05E9\u05D2\u05D9\u05D0\u05EA\
  \ \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8 (stderr) \u05D1-Dart \u05D6\u05D5 \u05E2\u05DC\
  \ \u05E9\u05DC\u05D9\u05D7\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\
  \u05D9\u05D0\u05D4 \u05D5\u05D0\u05D1\u05D7\u05D5\u05E0\u05D9\u05DD \u05DC\u05D6\
  \u05E8\u05DD \u05E0\u05E4\u05E8\u05D3, \u05D1\u05E0\u05E4\u05E8\u05D3 \u05DE\u05D4\
  \u05E4\u05DC\u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (stdout). \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D3\u05D9\u05DC \u05D1\u05D9\u05DF \u05E4\
  \u05DC\u05D8\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבה אל שגיאת סטנדרט (stderr) ב-Dart זו על שליחת הודעות שגיאה ואבחונים לזרם נפרד, בנפרד מהפלט הסטנדרטי (stdout). תכנתים עושים זאת כדי להבדיל בין פלט תכנית רגיל לבין שגיאות או הודעות אזהרה, מה שמאפשר ניפוי באגים ורישום יותר קלים.

## איך לעשות:

ב-Dart, הכתיבה אל stderr היא פשוטה באמצעות האובייקט `stderr` הזמין ב-`dart:io`. הנה דוגמא פשוטה:

```dart
import 'dart:io';

void main() {
  stderr.writeln('This is an error message.');
}
```

הפלט כאשר מריצים:
```
This is an error message.
```
הודעה זו נשלחת אל זרם ה-stderr, אשר נצפה בדרך כלל בקונסול או בטרמינל.

כדי להדגים סיבוך רב יותר, כמו רישום יוצא מן הכלל, מערכת התכונות העשירה של Dart מאפשרת טיפול בשגיאות קונציזי ויעיל:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // לדמות פעולה שעלולה לזרוק
    throw Exception('Something went wrong!');
  } catch (e) {
    stderr.writeln('Error: $e');
  }
}

void main() {
  riskyOperation();
}
```

הפלט כאשר מריצים:
```
Error: Exception: Something went wrong!
```

תבנית זו שימושית במיוחד עבור אפליקציות שצריכות להפריד בין רישומים רגילים לבין רישומי שגיאה, מה שמקל על הניטור וניפוי באגים של אפליקציות.

למרות שהספרייה הסטנדרטית של Dart היא די מקיפה, רבות מהתכניות לא דורשות ספריות צד שלישי לכתיבה אל stderr. עם זאת, אם לאפליקציה שלך נדרשות יכולות רישום מתוחכמות יותר (למשל, לקבצים, דרך הרשת, עיצוב), החבילה `logging` היא בחירה פופולרית. הנה מבט מהיר על שימוש ב-`logging` לשגיאות:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Severe Error: Something significantly bad happened.');
}
```

הפלט כאשר מריצים:
```
SEVERE: 2023-04-01 00:00:00.000: Severe Error: Something significantly bad happened.
```

שיטה זו מציעה רמה גבוהה יותר של התאמה אישית ושליטה על מה שנרשם כשגיאה ואיך זה מעוצב, מה שיכול לעזור מאוד באפליקציות גדולות ומורכבות יותר.
