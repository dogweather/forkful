---
title:                "שרשור מחרוזות"
aliases:
- /he/arduino/concatenating-strings/
date:                  2024-01-20T17:34:24.507288-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
השרשור של מחרוזות הוא תהליך שבו מחברים שתי מחרוזות או יותר לאחת. תכניתנים עושים זאת כדי ליצור טקסט דינמי, לעיצוב פלט ולשלב נתונים במשפטים.

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
