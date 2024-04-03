---
date: 2024-01-20 17:57:31.678462-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05E0\u05E0\
  \u05D9\u05D7 \u05E8\u05D5\u05E6\u05D9\u05DD \u05DC\u05E9\u05E0\u05D5\u05EA \u05DE\
  \u05D9\u05DC\u05D4 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA. \u05D1\u05D3\u05D5\
  \u05D2\u05DE\u05D4 \u05D6\u05D5, \u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD \u05D0\
  \u05EA \"World\" \u05D1\"Arduino\". \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4\
  \ `replace()` \u05EA\u05E2\u05E9\u05D4 \u05D0\u05EA \u05D4\u05E2\u05D1\u05D5\u05D3\
  \u05D4 \u05E2\u05D1\u05D5\u05E8\u05E0\u05D5."
lastmod: '2024-03-13T22:44:39.743109-06:00'
model: gpt-4-1106-preview
summary: "\u05E0\u05E0\u05D9\u05D7 \u05E8\u05D5\u05E6\u05D9\u05DD \u05DC\u05E9\u05E0\
  \u05D5\u05EA \u05DE\u05D9\u05DC\u05D4 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  ."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
weight: 10
---

## How to: (איך לעשות:)
נניח רוצים לשנות מילה במחרוזת. בדוגמה זו, מחליפים את "World" ב"Arduino". הפונקציה `replace()` תעשה את העבודה עבורנו.
```Arduino
String text = "Hello World";
text.replace("World", "Arduino");
Serial.begin(9600);
Serial.println(text); // "Hello Arduino" יודפס למסוף
```

## Deep Dive (צלילה לעומק)
היכולת לחיפוש והחלפה ב-Arduino היא בסיסית למדי. בעבר, ללא מחלקת `String`, היינו צריכים להשתמש בפונקציות כמו `strcat()`, `strcmp()`, ו-`memcpy()` של C. אכן, תמיד יש את האופציה לחזור לשיטות אלו לביצועיות טובה יותר. אבל עבור המתחילים, `String` ב-Arduino מספק דרך יעילה ופשוטה יחסית לעבודה עם מחרוזות. יש לזכור כי שימוש מופרז ב-`String` יכול להוביל לתופעת פיצול זיכרון, ולכן עדיף להימנע משימוש חוזר ונשנה בפונקציות החלפה בתוכניות ארוכות ומורכבות.

## See Also (ראה גם)
- [Arduino Reference: String Replace](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Arduino String Tutorial](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringConstructors)
