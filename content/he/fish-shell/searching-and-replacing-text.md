---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט הם פעולות בסיסיות שמתכנתים עושים בשפות תכנות שונות. הם משמשים ביישום משובח של שינויים בנתונים, לחסוך זמן ולמזער את הסיכוי לשגיאות.

## איך ל:
אפשר לחפש ולהחליף ב-Fish Shell כך:

```Fish Shell
echo 'שלום, עולם!' | string replace 'עולם' 'דג'
```
הפלט המנוסח שונית יהיה 'שלום, דג!'.

## צלילה עמוקה
Fish Shell הוא קדם וחפש בראש זריעה של Bourne Shell, שפת התכנות שממנה נכתבות Unix במקור. חלופות לפקודת 'string replace' משרתת Fish הן פקודות ה-sed או awk של Unix, שהן כמובן אקלכטיות יותר. פרטי המימוש: Fish משתמשת בספריה של C++ כדי לבצע את רוב הפעולות שלה, כולל 'string replace'.

## ראה גם
- [דוקומנטציה לפקודת Fish string replace](https://fishshell.com/docs/current/cmds/string-replace.html)
- [טוטוריאל חיפוש והחלפה עם sed](https://www.grymoire.com/Unix/Sed.html)
- [מדריך awk של GNU](https://www.gnu.org/software/gawk/manual/gawk.html)