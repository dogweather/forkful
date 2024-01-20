---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפת טקסט הוא התהליך שבו נמצאים מופעים של מחרוזת מסוימת ונמחלפות במחרודה אחרת. מתכנתים עושים זאת לשיפור, תיקון או שדרוג קוד, או לפעמים כדי להתאים למקרים משתנים.

## כיצד:

אם אנחנו רוצים לחפש ולהחליף מחרוזת ב-Arduino, אנו עושим את זה באמצעות מתודה של String נקראת ```replace()```. הפונקציה מקבלת שני ארגומנטים: המחרוזת לחיפוש והמחרוזת שאיתה להחליף.

```Arduino
String s = "שלום עולם";
s.replace("עולם", "ארדואינו");
Serial.begin(9600);
Serial.println(s); // "שלום ארדואינו"
```

## חטיבה עמוקה:

1. טקסט מחפש ומחליף הוא מערכת שנמצאה במערכות תכנות מאז שורתה של מחשבים. 

2. ניתן גם לבצע זאת באמצעות מתודות של הפעולות ```indexOf()``` ו- ```substring()```, אך ```replace()``` הוא הדרך המהירה והפשוטה ביותר. 

3. ב- Arduino, הפונקציה ```replace()``` מחזירה ```void```, מה שאומר שהיא משנה את המחרוזת שלך "במקום" ללא החזרת ערך.

## ראו גם:

1. [מערכת Arduino Reference: String](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
2. [Arduino Forum: עזרה ב- replace()](https://forum.arduino.cc/index.php?topic=593582.0)
3. [מדריך String Manipulation ל-Arduino](https://programmingelectronics.com/an-arduino-string-manipulation-arduino-to-arduino-tutorial/)