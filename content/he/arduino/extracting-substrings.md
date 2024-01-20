---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
חילוץ מחרוזת תת-מחרוזת הוא הפעולה של קבלת חלק ממחרוזת. מתכנתים משתמשים בה כדי לנתח, לניתוח או לשלוט על נתונים באופן ספציפי.

## איך לעשות את זה:
עשויים לחילץ תת-מחרוזות ב-Arduino בעזרת הפונקציה `substring()`. 
לדוגמה:
```Arduino
String sentence = "Hello, Arduino Programmers!";
String extract = sentence.substring(7, 15);
Serial.println(extract);
```
הפלט של הדוגמה הזאת יהיה:
```Arduino
"Arduino P"
```
## צלילה עמוקה
השאילתה `substring()` מגיעה מתיחסים האובייקטים של שפת ה-Java.
אלטרנטיבות כוללות שימוש בפונקציות כמו `strtok()` או `sscanf()` בשפות שאינן תומכות באובייקטים.
עידן המידע שלנו מחייב ניתוח ותנאליזה מחרוזות תוך כדי פעולה, ואילו פונקציות מאפשרות למתכנתים לבצע משימות אלו באופן יעיל.

## ראה גם
- [מדריך לשפת בראשית Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [ממונחים מתכנתים: מחרוזות](https://en.wikipedia.org/wiki/String_(computer_science))
- [דוגמאות שימוש של `substring()`](http://string-functions.com/arduino-string-substring.aspx)