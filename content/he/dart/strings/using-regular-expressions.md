---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:59.674857-07:00
description: "\u05D0\u05D9\u05DA \u05DC: Dart \u05DE\u05E9\u05EA\u05DE\u05E9\u05EA\
  \ \u05D1\u05DB\u05D9\u05EA\u05D4 `RegExp` \u05E2\u05D1\u05D5\u05E8 \u05D1\u05D9\u05D8\
  \u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD. \u05D4\
  \u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D0 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA\
  \ \u05DC\u05D4\u05EA\u05D0\u05DE\u05EA \u05D3\u05E4\u05D5\u05E1 \u05E4\u05E9\u05D5\
  \u05D8 \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA."
lastmod: '2024-03-13T22:44:38.828059-06:00'
model: gpt-4-0125-preview
summary: "Dart \u05DE\u05E9\u05EA\u05DE\u05E9\u05EA \u05D1\u05DB\u05D9\u05EA\u05D4\
  \ `RegExp` \u05E2\u05D1\u05D5\u05E8 \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\
  \u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D9\u05DC\u05D9\u05DD"
weight: 11
---

## איך ל:
Dart משתמשת בכיתה `RegExp` עבור ביטויים רגולריים. הנה דוגמא בסיסית להתאמת דפוס פשוט בתוך מחרוזת:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Learning Dart programming is exciting.';

  if (pattern.hasMatch(text)) {
    print('נמצאה התאמה!');
  } else {
    print('לא נמצאה התאמה.');
  }
  // פלט: נמצאה התאמה!
}
```

כדי לחלץ התאמות ממחרוזת, אתה יכול להשתמש בשיטה `allMatches`. שיטה זו מחזירה איטרבל של התאמות:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart is awesome!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // מדפיס את המחרוזות שהתאימו.
  }
  // פלט:
  // Dart
  // is
  // awesome
}
```

החלפת טקסט ניתן להשיג באמצעות השיטות `replaceFirst` או `replaceAll`:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart is not just a dart.';
  
  // החלפת המופע הראשון
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // פלט: Flutter is not just a dart.

  // החלפת כל המופעים
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // פלט: Flutter is not just a flutter.
}
```

פיצול מחרוזת לפי דפוס regex הוא פשוט באמצעות שימוש בשיטת `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // מתאים לכל תווי רווח
  var text = 'Dart is fun';

  var parts = text.split(pattern);
  print(parts); 
  // פלט: [Dart, is, fun]
}
```

לעיבוד או אימותים מורכבים שלא נתמכים ישירות על ידי `RegExp` של Dart, ייתכן שתרצה לשקול ספריות צד שלישי, אך ספריית הסטנדרט של Dart לעיתים קרובות מספיקה למשימות regex נפוצות, תוך הדגשת השימושיות והגמישות שלה בטיפול בביטויים רגולריים.
