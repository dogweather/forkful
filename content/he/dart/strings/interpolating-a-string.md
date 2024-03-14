---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:21.323483-07:00
description: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\
  \u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05DE\u05D6\u05E8\u05D9\u05E7\u05D9\u05DD\
  \ \u05E2\u05E8\u05DB\u05D9\u05DD \u05E9\u05DC \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD\
  \ \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DC\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA, \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\
  \u05D1\u05D5\u05EA \u05D1\u05E9\u05D1\u05D9\u05DC \u05DC\u05D9\u05E6\u05D5\u05E8\
  \ \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05DE\u05E9\u05DE\u05E2\u05D5\u05EA\u05D9\
  \u05D5\u05EA \u05D1\u05DC\u05D9 \u05E6\u05D5\u05E8\u05DA \u05D1\u05D7\u05D9\u05D1\
  \u05D5\u05E8\u05D9\u05DD \u05DE\u05E1\u05D5\u05E8\u05D1\u05DC\u05D9\u05DD.\u2026"
lastmod: '2024-03-13T22:44:38.821071-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\
  \u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05DE\u05D6\u05E8\u05D9\u05E7\u05D9\u05DD\
  \ \u05E2\u05E8\u05DB\u05D9\u05DD \u05E9\u05DC \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD\
  \ \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DC\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA, \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\
  \u05D1\u05D5\u05EA \u05D1\u05E9\u05D1\u05D9\u05DC \u05DC\u05D9\u05E6\u05D5\u05E8\
  \ \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05DE\u05E9\u05DE\u05E2\u05D5\u05EA\u05D9\
  \u05D5\u05EA \u05D1\u05DC\u05D9 \u05E6\u05D5\u05E8\u05DA \u05D1\u05D7\u05D9\u05D1\
  \u05D5\u05E8\u05D9\u05DD \u05DE\u05E1\u05D5\u05E8\u05D1\u05DC\u05D9\u05DD.\u2026"
title: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

אינטרפולציה של מחרוזת היא התהליך שבו מזריקים ערכים של משתנים ישירות לתוך מחרוזות, לעיתים קרובות בשביל ליצור הודעות משמעותיות בלי צורך בחיבורים מסורבלים. תכנתים עושים את זה כדי לקבל קוד נקי יותר, קריא יותר וכדי למנוע שגיאות שנוטות להתרחש בחיבורי מחרוזות מורכבים.

## איך לעשות:

ב-Dart, אינטרפולציה של מחרוזת היא פשוטה, תוך שימוש בסימן `$` לאינטרפולציה של ביטויים ישירות בתוך מילוליונים של מחרוזות:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // אינטרפולציה פשוטה של משתנה
  print('Learning $name in $year!');
  // פלט: Learning Dart in 2023!
  
  // אינטרפולציה של ביטויים
  print('In two years, it will be ${year + 2}.');
  // פלט: In two years, it will be 2025.
}
```

במקרה שיש לכם ביטויים יותר מורכבים או שאתם רוצים לבצע פעולות בתוך המחרוזת עצמה, סגרו את הביטוי ב`${}`. ל-Dart אין ספריות צד שלישי פופולריות במיוחד עבור אינטרפולציה של מחרוזת כיוון שהוא מאובזר היטב מלכתחילה להתמודד עם תסריטים מגוונים ומורכבים.
