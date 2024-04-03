---
date: 2024-01-20 17:50:23.913812-07:00
description: "How to: \u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.744549-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 8
---

## How to: איך לעשות:
```Arduino
char name[] = "דני";
int age = 30;
char buffer[50];

// פורמטים ומילוי תבנית
sprintf(buffer, "שלום, קוראים לי %s ואני בן %d.", name, age);

// הדפסת התוכן למסוף הסיריאלי
Serial.begin(9600);
Serial.println(buffer);

// הדפסת התוצאה:
// שלום, קוראים לי דני ואני בן 30.
```
זכרו להתחיל תקשורת סיריאלית לפני הדפסה למסוף.

## Deep Dive צלילה עמוקה:
מילוי תבנית מתמצאת בשפות רבות ונולדה מצורך לערבב טקסטים ומשתנים בצורה נוחה. ב-Arduino, `sprintf` היא הפונקציה המאפשרת זאת. חלופות כוללות שימוש במחלקת `String` עם אופרטורים כמו `+` לחיבור מחרוזות, אבל זו גישה יקרה יותר בזיכרון. `sprintf` אמנם יציבה, אבל דורשת זהירות כדי למנוע חריגה מגבולות המערך.

## See Also ראה גם:
- מדריך לפונקציית `sprintf`: [CPlusPlus.com](http://www.cplusplus.com/reference/cstdio/sprintf/)
- מידע נוסף על מחלקת `String` ב-Arduino: [Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
