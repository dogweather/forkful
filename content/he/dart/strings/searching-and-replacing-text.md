---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:51.032004-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05D9\u05DD \u05D1\
  \u05D3\u05D9\u05E7\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DB\u05D3\
  \u05D9 \u05DC\u05DE\u05E6\u05D5\u05D0 \u05D3\u05E4\u05D5\u05E1\u05D9\u05DD \u05DE\
  \u05E1\u05D5\u05D9\u05DE\u05D9\u05DD \u05D0\u05D5 \u05E8\u05E6\u05E4\u05D9 \u05EA\
  \u05D5\u05D5\u05D9\u05DD \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\u05DD \u05D1\u05EA\
  \u05D5\u05DB\u05DF \u05D7\u05D3\u05E9. \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5\
  \ \u05D4\u05D9\u05D0 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05DC\u05DE\u05E9\u05D9\
  \u05DE\u05D5\u05EA \u05DB\u05DE\u05D5 \u05D0\u05D9\u05DE\u05D5\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD,\u2026"
lastmod: '2024-03-13T22:44:38.819080-06:00'
model: gpt-4-0125-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05D9\u05DD \u05D1\u05D3\
  \u05D9\u05E7\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05DE\u05E6\u05D5\u05D0 \u05D3\u05E4\u05D5\u05E1\u05D9\u05DD \u05DE\u05E1\
  \u05D5\u05D9\u05DE\u05D9\u05DD \u05D0\u05D5 \u05E8\u05E6\u05E4\u05D9 \u05EA\u05D5\
  \u05D5\u05D9\u05DD \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\u05DD \u05D1\u05EA\u05D5\
  \u05DB\u05DF \u05D7\u05D3\u05E9."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
weight: 10
---

## מה ולמה?

חיפוש והחלפת טקסט ב-Dart כוללים בדיקת מחרוזות כדי למצוא דפוסים מסוימים או רצפי תווים והחלפתם בתוכן חדש. פעולה זו היא בסיסית למשימות כמו אימות נתונים, עיצוב פלט, ניתוח קלט משתמש, או אפילו שינוי ב-URLים ונתיבי קבצים, הופכת אפליקציות ליותר דינמיות ותגובתיות לצרכי המשתמש.

## איך לעשות:

Dart מספקת שיטות חזקות לחיפוש והחלפת טקסט ישירות דרך מחלקת ה-`String` שלה, ללא צורך בספריות חיצוניות. הנה איך אפשר לעשות זאת:

### חיפוש והחלפה בסיסיים

לחיפוש תת-מחרוזת והחלפתה במחרוזת אחרת, ניתן להשתמש ב-`replaceAll`:

```dart
String sampleText = "Hello, Dart! Dart is great.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // פלט: Hello, Flutter! Flutter is great.
```

### שימוש בביטויים רגולריים

לצרכי חיפוש והחלפה מורכבים יותר, Dart משתמשת בביטויים רגולריים דרך מחלקת ה-`RegExp`. זה מאפשר התאמת דפוסים והחלפה במחרוזות:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // פלט: Dart 2024, Flutter 2024
```

דוגמה זו מוצאת את כל המופעים של ספרה אחת או יותר (`\d+`) במחרוזת ומחליפה אותם ב-"2024".

### חיפוש לא רגיש לרישיות

לביצוע חיפוש שאינו רגיש לרישיות, ניתן לשנות את בנאי ה-`RegExp` כך שיתעלם מהרישיות:

```dart
String sampleText = "Welcome to Dart, the programming language.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // פלט: Welcome to Flutter, the programming language.
```

### החלפה באמצעות פונקציה

להחלפות דינמיות בהתבסס על ההתאמה עצמה, Dart מאפשרת להעביר פונקציה ל-`replaceAllMapped`. פונקציה זו יכולה לבצע פעולות או חישובים על הרצפים שהתאימו:

```dart
String sampleText = "Increment 5 by 1 to get 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // פלט: Increment 6 by 1 to get 7.
```

פעולה זו מחליפה כל רצף של ספרות בערכו המוגדל. כל התאמה מתורגמת למספר שלם, מוגדלת, ואז מומרת חזרה למחרוזת לצורך ההחלפה.

יכולות התמרת המחרוזות של Dart, במיוחד לחיפוש והחלפת טקסט, הופכות אותו לכלי עוצמתי לעיבוד והכנת נתונים בתוך האפליקציות שלכם. בין אם זה באמצעות החלפות מחרוזת ישירות או שימוש בכוחם של ביטויים רגולריים, Dart מספקת את הגמישות והביצועים הנדרשים לתמרת טקסט אפקטיבית.
