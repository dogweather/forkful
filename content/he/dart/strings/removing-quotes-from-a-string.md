---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:15.099307-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD\
  \ \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05D0\u05EA \u05D4\u05E1\u05E8\u05EA \u05E1\u05D9\u05DE\u05E0\u05D9 \u05D4\
  \u05E6\u05D9\u05D8\u05D5\u05D8 \u05D4\u05DB\u05E4\u05D5\u05DC\u05D9\u05DD (\") \u05D0\
  \u05D5 \u05D4\u05D9\u05D7\u05D9\u05D3\u05D9\u05DD (') \u05DE\u05EA\u05D7\u05D9\u05DC\
  \u05EA \u05D5\u05E1\u05D5\u05E3 \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA, \u05DE\
  \u05D4 \u05E9\u05DE\u05D5\u05E2\u05D9\u05DC \u05DC\u05E0\u05D9\u05E7\u05D5\u05D9\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05D4\u05DB\u05E0\u05EA \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3\u2026"
lastmod: '2024-03-13T22:44:38.824553-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05D0\u05EA \u05D4\u05E1\u05E8\u05EA \u05E1\u05D9\u05DE\u05E0\u05D9 \u05D4\u05E6\
  \u05D9\u05D8\u05D5\u05D8 \u05D4\u05DB\u05E4\u05D5\u05DC\u05D9\u05DD (\") \u05D0\u05D5\
  \ \u05D4\u05D9\u05D7\u05D9\u05D3\u05D9\u05DD (') \u05DE\u05EA\u05D7\u05D9\u05DC\u05EA\
  \ \u05D5\u05E1\u05D5\u05E3 \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA, \u05DE\u05D4\
  \ \u05E9\u05DE\u05D5\u05E2\u05D9\u05DC \u05DC\u05E0\u05D9\u05E7\u05D5\u05D9 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05D4\u05DB\u05E0\u05EA \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3\u2026"
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 9
---

## מה ולמה?
הסרת ציטוטים ממחרוזת ב-Dart כוללת את הסרת סימני הציטוט הכפולים (") או היחידים (') מתחילת וסוף המחרוזת, מה שמועיל לניקוי נתונים או הכנת מחרוזות לעיבוד נוסף. מתכנתים עושים זאת כדי לנרמל קלטי נתונים, להבטיח אחידות באחסון הנתונים, או כאשר הם מתממשקים עם API-ים שעשויים להחזיר נתונים בפורמטים מצוטטים.

## איך לעשות זאת:
Dart מספקת דרכים ישירות להסיר ציטוטים ממחרוזת באמצעות שיטות מחרוזת מובנות ללא צורך בספריות צד שלישי.

### דוגמה 1: באמצעות `replaceFirst` ו-`replaceAll`
אם אתה מתמודד עם מחרוזות שמתחילות ומסתיימות בציטוטים, תוכל להשתמש בשיטות `replaceFirst` ו-`replaceAll` כדי להסיר אותם.

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart Programming\'';

// הסרת ציטוטים כפולים
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // פלט: Hello, World!

// הסרת ציטוטים יחידים
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // פלט: Dart Programming
```

### דוגמה 2: באמצעות `substring`
שיטה זו שימושית כאשר אתה בטוח שהציטוטים נמצאים בדיוק בתחילת ובסוף המחרוזת.

```dart
String quotedString = '"Flutter Development"';
// בדוק אם מתחיל ומסתיים בציטוטים לפני הסרה כדי למנוע שגיאות
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // פלט: Flutter Development
```

### דוגמה 3: שיטת הרחבה מותאמת אישית
לשימושיות רבה יותר, במיוחד אם הפרויקט שלך כולל הסרת ציטוטים באופן תדיר, שקול ליצור שיטת הרחבה מותאמת אישית על `String`.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // פלט: This is Dart
  print(singleQuoted.unquote()); // פלט: This is awesome
}
```

התקרבויות אלו אמורות לעזור לך להסיר ציטוטים ממחרוזות ביעילות ב-Dart, מה שישפר את זרימות העבודה שלך בעיבוד והכנת נתונים.
