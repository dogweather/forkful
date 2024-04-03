---
date: 2024-01-20 17:38:03.389632-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\
  \u05D5\u05EA \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4 \u05E0\u05D7\u05D5\u05E5? \u05D6\
  \u05D5 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05E9\u05E0\u05D4 \u05D0\u05EA\
  \ \u05DB\u05DC \u05EA\u05D5\u05D5\u05D9 \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA. \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D7\u05D3\
  \ \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD, \u05DC\u05D4\u05E7\u05DC \u05E2\u05DC\
  \ \u05D4\u05E9\u05D5\u05D5\u05D0\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.746086-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\
  \u05EA \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4 \u05E0\u05D7\u05D5\u05E5."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

## What & Why?
מה זה המרת מחרוזת לאותיות קטנות ולמה זה נחוץ? זו פעולה שמשנה את כל תווי המחרוזת לאותיות קטנות. תכנתים עושים זאת לעיתים כדי לאחד פורמטים, להקל על השוואת מחרוזות, או להתאים טקסט לסטנדרטים קונקרטיים.

## How to:
מוצגים כאן כמה דוגמאות קוד שמראות איך להמיר מחרוזת לאותיות קטנות ב-Arduino:

```Arduino
String original = "Hello, World!";
String lowerCase = original.toLowerCase();

Serial.begin(9600);
Serial.println(lowerCase); // ידפיס: hello, world!
```

קוד זה ישתמש בפונקציה `.toLowerCase()` כדי להמיר את המחרוזת במשתנה `original` לאותיות קטנות ולאחסן אותה במשתנה `lowerCase`.

## Deep Dive:
ההמרה של מחרוזות לאותיות קטנות היא פרקטיקה קלאסית בתכנות, נפוצה מראשית ימי המחשב האישי. בארדואינו, זה קל במיוחד כי השפה מספקת פונקציית מובנית עבור פעולה זו. אלטרנטיבות כוללות שימוש בלולאות לטיוב כל תו לאות קטנה באופן ידני תוך שימוש בטבלת ASCII. פרטי היישום כוללים התייחסות למקרים שוליים כמו תווים מיוחדים ושפות שונות שמרבה עליהן הפונקציה `.toLowerCase()`.

## See Also:
עיון נוסף בנושאים דומים ניתן למצוא במקורות הבאים:
- [ASCII Table and Description](http://www.asciitable.com/)
- [Arduino String Manipulation](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringCharacters)
