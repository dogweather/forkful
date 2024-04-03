---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:39.926634-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\
  \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05DE\
  \u05E1\u05D5\u05D9\u05DE\u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D7\u05D9\u05D5\u05E0\u05D9 \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD, \u05D7\u05D9\u05D8\u05D5\u05D9 \u05D0\u05D5 \u05DB\u05D0\
  \u05E9\u05E8 \u05DE\u05DB\u05D9\u05E0\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8 \u05DC\
  \u05E2\u05D9\u05D1\u05D5\u05D3 \u05E0\u05D5\u05E1\u05E3. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05DE\u05E9\u05D9\u05DE\
  \u05D4 \u05D6\u05D5 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D5\u05D5\u05D3\u05D0\
  \ \u05D0\u05EA \u05E9\u05DC\u05DE\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.817277-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05DE\u05E1\
  \u05D5\u05D9\u05DE\u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D7\
  \u05D9\u05D5\u05E0\u05D9 \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD, \u05D7\u05D9\u05D8\u05D5\u05D9 \u05D0\u05D5 \u05DB\u05D0\u05E9\
  \u05E8 \u05DE\u05DB\u05D9\u05E0\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8 \u05DC\u05E2\
  \u05D9\u05D1\u05D5\u05D3 \u05E0\u05D5\u05E1\u05E3."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05D3\u05E4\u05D5\u05E1"
weight: 5
---

## איך לעשות:
Dart מפשטת את התהליך של הסרת תווים התואמים לתבנית מוגדרת מראש באמצעות ביטויים רגולריים ושימוש במתודה `replaceAll`. אין צורך בספריות צד שלישי לשימוש בסיסי, ובכך הופך את הגישה הזו לנגישה מאוד.

להלן דוגמא פשוטה המדגימה כיצד להסיר ספרות ממחרוזת:

```dart
void main() {
  String stringWithDigits = 'Dart123 is fun456';
  // הגדרת תבנית ביטוי רגולרי התואמת לכל הספרות
  RegExp digitPattern = RegExp(r'\d');
  
  // החלפת כל התופעות של התבנית במחרוזת ריקה
  String result = stringWithDigits.replaceAll(digitPattern, '');
  
  print(result); // פלט: Dart is fun
}
```

בהנחה שאתם מתמודדים עם תרחיש מורכב יותר, כמו להסיר תווים מיוחדים למעט רווחים ופיסוק. כך תעשו זאת:

```dart
void main() {
  String messyString = 'Dart!@# is *&()fun$%^';
  // הגדרת תבנית התואמת הכל פרט לאותיות, מספרים, רווחים ופיסוק
  RegExp specialCharPattern = RegExp(r'[^a-zA-Z0-9 \.,!?]');
  
  String cleanedString = messyString.replaceAll(specialCharPattern, '');
  
  print(cleanedString); // פלט: Dart! is fun
}
```

למשימות הדורשות התאמת תבניות והחלפה מתקדמות יותר, תיעוד המחלקה `RegExp` המקיף של Dart מציע צלילה עמוקה לתוך ביטויים מורכבים יותר והשימוש בהם. עם זאת, הדוגמאות המוצגות לעיל מכסות את רוב המקרים הקריטיים למחיקת תווים בהתבסס על תבניות בתכנות Dart.
