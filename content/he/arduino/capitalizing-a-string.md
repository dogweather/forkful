---
title:                "הגדלת תו מחרוזת"
html_title:           "Arduino: הגדלת תו מחרוזת"
simple_title:         "הגדלת תו מחרוזת"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# למה
בכדי לשנות אותיות מהכתובת קטנות לגדולות במחשב ארדוינו, ניתן להשתמש ב"capitalize" שבשפת תכנות. פעולה זו יכולה להיות שימושית לטפל בשגיאות שתוצרו כאשר משתמשים מקלידים באותיות קטנות במקום גדולות ולהציג את כל התווים קבועים לגודלם המקורי.

# איך לעשות זאת
```Arduino
String str = "hello world";
str.capitalize();
Serial.println(str);
```
```
HELLO WORLD
```

כדי לבצע המרה לאותיות גדולות, יש להשתמש בפעולת "capitalize" על אובייקט סטרינג ולהשתמש בפקודת "Serial" כדי להדפיס את התוצאה. ניתן לבצע פעולה זו על כל מחרוזת בתכנית וכך לטפל בתווים קטנים וגדולים כאחד.

# הכנסה לעומק
פעולת "capitalize" עוברת על כל תו במחרוזת וממירה את התו לאות גדולה אם התו המקורי הוא אות קטנה. אם התו אינו אות, הוא יישאר כמו שהוא. חשוב לציין כי פעולה זו משנה את המחרוזת המקורית ואינה מחזירה ערך חדש. על כן, כדאי לשמור את התוצאה במשתנה חדש אם זקוקים למחרוזת מקורית במקום הניסוי.

# ראה גם
- תיעוד רשמי על capitalize: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/capitalize/ 
- מדריך להתחיל עם ארדוינו: https://www.youtube.com/watch?v=iVHm_WY-4ZA 
- קהילת ארדוינו הרשמית: https://forum.arduino.cc/