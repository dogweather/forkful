---
title:                "כתיבת בדיקות"
aliases: - /he/elm/writing-tests.md
date:                  2024-02-03T19:31:00.042041-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת טסטים ב-Elm כוללת יצירת מקרי בדיקה לאימות נכונות הקוד שלך ב-Elm, ולוודא שהוא מתנהג כפי שציפית. מתכנתים עושים זאת כדי לאתר באגים במוקדם, להקל על תחזוקה, ולשפר את האיכות והאמינות של היישומים שלהם.

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
