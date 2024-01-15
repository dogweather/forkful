---
title:                "צירוף מחרוזות"
html_title:           "Haskell: צירוף מחרוזות"
simple_title:         "צירוף מחרוזות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

למה לכתוב על כך איך לחבר מחרוזות ב־Haskell? כי זה מייצג תחביר חיוני בשפת תכנות זו, ולהשתמש בו נדרש בכמעט כל קוד.

## כיצד לעשות זאת

כדי לחבר מחרוזות ב־Haskell, יש להשתמש ביעד חשבוני `++`, שמייצג פעולת חיבור בין שני מחרוזות. לדוגמה:

```Haskell
"Hello " ++ "world!" 
```

והדפסה של זה יחזיר:

```Haskell
"Hello world!" 
```

אם רוצים לחבר מעל לשני מחרוזות, ניתן להשתמש בפונקציה `concat`, כך:

```Haskell
concat ["Hello ", "my ", "name ", "is ", "Haskell."] 
```

והדפסה של זה תחזיר:

```Haskell
"Hello my name is Haskell."
```

כדי לחבר מחרוזות עם משתנים, יש להשתמש בפונקציה `show` כדי להמיר אותם למחרוזת. לדוגמה:

```Haskell
let name = "Haskell" 
"Hola " ++ show name 
```

והדפסה של זה תחזיר:

```Haskell
"Hola Haskell"
```

## ניחור עמוק

ניתן לעשות חיבור מחרוזות גם באמצעות פונקציות אחרות כמו `printf` או `concatMap`. כמו כן, ניתן לשחק עם המעטפת גם בשורת פקודה של Haskell כדי לחבר מחרוזות באופן מהיר ויעיל. כל אלו נותנים למתכנתים יכולת נרחבת יותר ומגוונת יותר לחיבור מחרוזות מתוך קוד ביצירתי.

## ראה גם

- העמקת במושגים של מחרוזות ב־Haskell: https://www.tutorialspoint.com/haskell/haskell_strings.htm
- פונקציות חשבוניות נוספות ב־Haskell: https://wiki.haskell.org/Common_functions#String_manipulation_functions