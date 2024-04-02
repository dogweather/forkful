---
date: 2024-01-20 17:47:04.589362-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05D0\u05D5\u05E8\u05DA \u05D4\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4 \u05D7\u05E9\u05D5\
  \u05D1? \u05D0\u05D5\u05E8\u05DA \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\
  \u05E1\u05E4\u05E8 \u05DC\u05E0\u05D5 \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\
  \u05DD \u05D9\u05E9 \u05D1\u05D4. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD\
  \ \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DC\u05D5\u05D5\
  \u05D3\u05D0 \u05E9\u05D4\u05DB\u05E0\u05D9\u05E1\u05D4 \u05EA\u05E7\u05D9\u05E0\
  \u05D4, \u05DC\u05E2\u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D5\u05DC\
  \u05E9\u05DC\u05D5\u05D8 \u05E2\u05DC \u05D6\u05E8\u05D9\u05DE\u05D5\u05EA \u05DC\
  \u05D5\u05D2\u05D9\u05D5\u05EA."
lastmod: '2024-03-13T22:44:39.752586-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05D0\u05D5\u05E8\u05DA \u05D4\u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4 \u05D7\u05E9\u05D5\u05D1\
  ? \u05D0\u05D5\u05E8\u05DA \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05E1\
  \u05E4\u05E8 \u05DC\u05E0\u05D5 \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD\
  \ \u05D9\u05E9 \u05D1\u05D4. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DC\u05D5\u05D5\u05D3\
  \u05D0 \u05E9\u05D4\u05DB\u05E0\u05D9\u05E1\u05D4 \u05EA\u05E7\u05D9\u05E0\u05D4\
  , \u05DC\u05E2\u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D5\u05DC\u05E9\
  \u05DC\u05D5\u05D8 \u05E2\u05DC \u05D6\u05E8\u05D9\u05DE\u05D5\u05EA \u05DC\u05D5\
  \u05D2\u05D9\u05D5\u05EA."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## What & Why?
מה זה אורך המחרוזת ולמה זה חשוב? אורך המחרוזת מספר לנו כמה תווים יש בה. תכניתנים משתמשים בזה לוודא שהכניסה תקינה, לעבד נתונים, ולשלוט על זרימות לוגיות.

## How to:
```Arduino
String myText = "שלום עולם";
int textLength = myText.length();

Serial.begin(9600);
Serial.print("אורך המחרוזת: ");
Serial.println(textLength); // יודפס: אורך המחרוזת: 10
```
בדוגמה, אנחנו משתמשים ב-`length()` כדי לקבוע את אורך המחרוזת, ואז מדפיסים אותו דרך ה-Serial Monitor.

## Deep Dive
בהיסטוריה של שפות תכנות, אורך המחרוזת תמיד היה נושא חשוב. בשפות כמו C, אורך מחרוזת נקבע על ידי חיפוש אחר התו המיוחד '\0'. בארדואינו, שמשתמש בשפת C++, יש למחלקה `String` מתודות מובנות כמו `length()`. חלפים ל-`length()` עלולים לכלול פונקציות כמו `strlen()` למערכי תווים מסוג char. המימוש של `length()` בעצם סופר את התווים עד שהוא מגיע לסיומת של המחרוזת.

## See Also
- תיעוד רשמי למתודת `length()` למחרוזות בארדואינו: [Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/)
- הבדלים בין מחלקת `String` למערכי `char` בארדואינו: [Arduino String vs. char](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringCharacters)
- מידע נוסף על קריאה וכתיבה דרך ה-Serial Monitor: [Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
