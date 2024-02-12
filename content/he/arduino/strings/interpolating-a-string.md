---
title:                "שרבוב מחרוזת"
aliases:
- /he/arduino/interpolating-a-string/
date:                  2024-01-20T17:50:23.913812-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/interpolating-a-string.md"
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
