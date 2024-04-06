---
date: 2024-01-20 17:34:24.507288-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05E2\u05D1\
  \u05E8, \u05D1\u05E2\u05D9\u05D3\u05DF \u05E9\u05DC \u05E9\u05E4\u05D5\u05EA \u05EA\
  \u05DB\u05E0\u05D5\u05EA \u05DE\u05D5\u05E7\u05D3\u05DE\u05D5\u05EA \u05DB\u05DE\
  \u05D5 C, \u05D4\u05E9\u05E8\u05E9\u05D5\u05E8 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05E0\u05E2\u05E9\u05D4 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\
  \u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DE\u05D5\u05E8\u05DB\
  \u05D1\u05D5\u05EA \u05DB\u05DE\u05D5 `strcat()`. \u05D1\u05D0\u05E8\u05D3\u05D5\
  \u05D0\u05D9\u05E0\u05D5, \u05D7\u05DC\u05D4 \u05D4\u05E4\u05D9\u05DB\u05D4 \u05DC\
  \u05E4\u05E9\u05D8\u05D5\u05EA \u05E2\u05DD \u05D4\u05DE\u05D7\u05DC\u05E7\u05D4\
  \u2026"
lastmod: '2024-04-05T22:50:53.854955-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E2\u05D1\u05E8, \u05D1\u05E2\u05D9\u05D3\u05DF \u05E9\u05DC \u05E9\
  \u05E4\u05D5\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05DE\u05D5\u05E7\u05D3\u05DE\
  \u05D5\u05EA \u05DB\u05DE\u05D5 C, \u05D4\u05E9\u05E8\u05E9\u05D5\u05E8 \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05E0\u05E2\u05E9\u05D4 \u05D1\u05D0\
  \u05DE\u05E6\u05E2\u05D5\u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA\
  \ \u05DE\u05D5\u05E8\u05DB\u05D1\u05D5\u05EA \u05DB\u05DE\u05D5 `strcat()`."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

## איך לעשות:
```Arduino
String stringOne = "שלום, ";
String stringTwo = "עולם!";
String combinedString = stringOne + stringTwo; // שרשור המחרוזות

Serial.begin(9600);
Serial.println(combinedString); // פלט: שלום, עולם!
```

מינוח לדוגמה:
```Arduino
String tempString = "הטמפרטורה היא: ";
int temp = 23; // נניח שזו הטמפרטורה שנמדדה
String tempWithUnits = tempString + temp + " מעלות צלזיוס"; // הוספת מספר למחרוזת ויחידות

Serial.println(tempWithUnits); // פלט: הטמפרטורה היא: 23 מעלות צלזיוס
```

## לעומק:
בעבר, בעידן של שפות תכנות מוקדמות כמו C, השרשור של מחרוזות נעשה באמצעות פונקציות מורכבות כמו `strcat()`. בארדואינו, חלה הפיכה לפשטות עם המחלקה `String` שמספקת יכולות שרשור נוחות.

לחלופין, אפשר להשתמש במערכי char ולעבוד עם פונקציות כמו `strcat()`, אבל זה דורש ניהול זיכרון קפדני והבנה טובה יותר של הקוד.

ברמת המימוש, השרשור של מחרוזות עלול לגרום לפיצול של הזיכרון (fragmentation). על תכניתנים להיות מודעים לכך, במיוחד במערכות עם משאבי זיכרון מוגבלים, כמו שכיח בפלטפורמת Arduino.

## ראו גם:
- מדריך למחלקת String באתר הרשמי של Arduino: [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- דיון בנושא שרשור מחרוזות וניהול זיכרון בפורום של Arduino: [Arduino Forum - Strings](http://forum.arduino.cc/index.php?topic=396450.0)
