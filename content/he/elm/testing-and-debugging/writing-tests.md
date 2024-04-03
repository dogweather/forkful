---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:00.042041-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D8\u05E1\u05D8\u05D9\u05DD \u05D1\
  -Elm \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E7\u05E8\
  \u05D9 \u05D1\u05D3\u05D9\u05E7\u05D4 \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA \u05E0\
  \u05DB\u05D5\u05E0\u05D5\u05EA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05D1\
  -Elm, \u05D5\u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05D5\u05D0 \u05DE\u05EA\
  \u05E0\u05D4\u05D2 \u05DB\u05E4\u05D9 \u05E9\u05E6\u05D9\u05E4\u05D9\u05EA. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05EA\u05E8 \u05D1\u05D0\u05D2\u05D9\u05DD\
  \ \u05D1\u05DE\u05D5\u05E7\u05D3\u05DD, \u05DC\u05D4\u05E7\u05DC \u05E2\u05DC\u2026"
lastmod: '2024-03-13T22:44:39.207753-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D8\u05E1\u05D8\u05D9\u05DD \u05D1-Elm\
  \ \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E7\u05E8\
  \u05D9 \u05D1\u05D3\u05D9\u05E7\u05D4 \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA \u05E0\
  \u05DB\u05D5\u05E0\u05D5\u05EA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05D1\
  -Elm, \u05D5\u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05D5\u05D0 \u05DE\u05EA\
  \u05E0\u05D4\u05D2 \u05DB\u05E4\u05D9 \u05E9\u05E6\u05D9\u05E4\u05D9\u05EA."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

## איך לעשות:
Elm משתמש בחבילה `elm-explorations/test` לכתיבת טסטים יחידה וטסטים מטושטשים. התחל בהוספת החבילה לפרויקט שלך:

```elm
elm install elm-explorations/test
```

צור קובץ טסט, למשל `tests/ExampleTest.elm`, ויבא את מודולי הבדיקה. הנה טסט פשוט שאומת פונקציה `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "A simple addition function"
        [ test "הוספת 2 ו-3 מניבה 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

כדי להריץ את הטסטים שלך, תצטרך את `elm-test`:

```shell
npm install -g elm-test
elm-test
```

זה יקומפל את הטסטים שלך וידפיס את התוצאות בטרמינל שלך. לדוגמה לעיל, הפלט צריך להיות משהו כזה:

```
נסיון הרצת הטסט עבר

משך זמן: 42 מילישניות
עברו:   1
נכשלו:   0
```

לדוגמה מורכבת יותר, נניח שאתה רוצה לטסט מטושטש את פונקציית ה`add` כדי לוודא שהיא מטפלת נכון במגוון רחב של קלטים של מספרים שלמים. היית שונה את ה`ExampleTest.elm` שלך באופן הבא:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "בדיקת פונקציית ההוספה באמצעות טשטוש"
        [ fuzz int "טסט מטושטש של הוספה עם שלמים אקראיים" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

הרץ שוב את `elm-test` כדי לראות את הטסטים המטושטשים בפעולה. הפלט ישתנה עם קלט אקראי אך טסטים מוצלחים יצביעו על אפס כשלונות:

```
נסיון הרצת הטסט עבר

משך זמן: 183 מילישניות
עברו:   100
נכשלו:   0
``` 

הדוגמאות הללו מראות איך לכתוב ולהריץ טסטים יחידה פשוטים וטסטים מטושטשים ב-Elm, באמצעות החבילה `elm-explorations/test`. טסטינג הוא חלק חשוב מתהליך הפיתוח, עוזר לוודא שהיישומים שלך ב-Elm אמינים ושומרים על איכות גבוהה.
