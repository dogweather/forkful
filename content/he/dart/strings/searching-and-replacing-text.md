---
title:                "חיפוש והחלפת טקסט"
date:                  2024-03-08T21:56:51.032004-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
