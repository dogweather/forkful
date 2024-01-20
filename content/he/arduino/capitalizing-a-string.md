---
title:                "הפיכת מחרוזת לאותיות גדולות"
html_title:           "Arduino: הפיכת מחרוזת לאותיות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות גדולות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה זה & למה?
בהון ציונים למעשה של הפיכת מחרוזת לאותיות ראשיות. מתכנתים משתמשים בזה כדי ליצור תחסית בין ערכים שונים קופצים.

## איך עושים
הנה דוגמא של קוד ופלט בקינונות של Arduino:

```Arduino
String myString = "hello world";
myString.toUpperCase();
Serial.println(myString); // Prints: "HELLO WORLD"
```

זהו קוד פשוט שמשנה את myString לאותיות ראשיות, ואז מדפיס את התוצאה.

## צליל עמוק
המרת מחרוזת לאותיות ראשיות היא אחת מהתוכנמות הבסיסיות בשפות תכנות רבות. ניתן גם להשתמש בדרכים אחרות לביצוע זה, למשל בעזרת ניתוח משולש עם Arduino. ניתן לחשוב על זה כאינדיקציה למעגל-אותיות:

```Arduino
String myString = "hello world";
for(int i = 0; i < myString.length(); i++){
   myString[i] = toupper(myString[i]);
}
Serial.println(myString); // Prints: "HELLO WORLD"
```

בקוד זה, אנו מעברים על כל אות במחרוזת ומשנים אותה לאות ראשונה.

## ראו גם
לינקים למקורות קשורים:

- אתר הבית של Arduino: https://www.arduino.cc/
- המדריך למתחילים של Arduino: https://www.arduino.cc/en/Guide/HomePage
- התיעוד של פונקציית toUpperCase: https://www.arduino.cc/en/Reference/StringToUpperCase
- מאמרים נוספים על תכנות Arduino: https://create.arduino.cc/projecthub?query=&category=&difficulty=&sort=trending&type=guide