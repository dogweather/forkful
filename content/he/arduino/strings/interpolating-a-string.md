---
date: 2024-01-20 17:50:23.913812-07:00
description: "\u05DE\u05D9\u05DC\u05D5\u05D9 \u05EA\u05D1\u05E0\u05D9\u05EA \u05E2\
  \u05DD \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D5\u05D0 \u05E9\u05D9\
  \u05D8\u05D4 \u05DC\u05D4\u05DB\u05E0\u05D9\u05E1 \u05DE\u05E9\u05EA\u05E0\u05D9\
  \u05DD \u05DC\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D8\u05E7\
  \u05E1\u05D8. \u05EA\u05DB\u05E0\u05EA\u05D9 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E9\u05D5\u05EA \u05E7\u05D5\
  \u05D3 \u05D2\u05DE\u05D9\u05E9 \u05D9\u05D5\u05EA\u05E8 \u05D5\u05DC\u05D9\u05D9\
  \u05E6\u05E8 \u05DE\u05D9\u05D3\u05E2 \u05D3\u05D9\u05E0\u05DE\u05D9 \u05DC\u05DE\
  \u05E9\u05EA\u05DE\u05E9."
lastmod: '2024-02-25T18:49:37.984620-07:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D9\u05DC\u05D5\u05D9 \u05EA\u05D1\u05E0\u05D9\u05EA \u05E2\u05DD\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D5\u05D0 \u05E9\u05D9\u05D8\
  \u05D4 \u05DC\u05D4\u05DB\u05E0\u05D9\u05E1 \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD\
  \ \u05DC\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D8\u05E7\u05E1\
  \u05D8. \u05EA\u05DB\u05E0\u05EA\u05D9 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E9\u05D5\u05EA \u05E7\u05D5\u05D3\
  \ \u05D2\u05DE\u05D9\u05E9 \u05D9\u05D5\u05EA\u05E8 \u05D5\u05DC\u05D9\u05D9\u05E6\
  \u05E8 \u05DE\u05D9\u05D3\u05E2 \u05D3\u05D9\u05E0\u05DE\u05D9 \u05DC\u05DE\u05E9\
  \u05EA\u05DE\u05E9."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## What & Why? מה ולמה?

מילוי תבנית עם מחרוזות הוא שיטה להכניס משתנים לתוך מחרוזת טקסט. תכנתי עושים את זה כדי לעשות קוד גמיש יותר ולייצר מידע דינמי למשתמש.

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
