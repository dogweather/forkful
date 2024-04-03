---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:21.323483-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Dart, \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E9\u05D5\u05D8\u05D4\
  , \u05EA\u05D5\u05DA \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E1\u05D9\u05DE\u05DF\
  \ `$` \u05DC\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D9\u05E9\u05D9\u05E8\u05D5\
  \u05EA \u05D1\u05EA\u05D5\u05DA \u05DE\u05D9\u05DC\u05D5\u05DC\u05D9\u05D5\u05E0\
  \u05D9\u05DD \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA."
lastmod: '2024-03-13T22:44:38.821071-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Dart, \u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\
  \u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\
  \u05E9\u05D5\u05D8\u05D4, \u05EA\u05D5\u05DA \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\
  \u05E1\u05D9\u05DE\u05DF `$` \u05DC\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\
  \u05E6\u05D9\u05D4 \u05E9\u05DC \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D9\
  \u05E9\u05D9\u05E8\u05D5\u05EA \u05D1\u05EA\u05D5\u05DA \u05DE\u05D9\u05DC\u05D5\
  \u05DC\u05D9\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA."
title: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 8
---

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
