---
title:                "Haskell: קריאת פרמטרים משורת הפקודה"
simple_title:         "קריאת פרמטרים משורת הפקודה"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

בעולם התכנות, להתמודד עם פרמטרי שורת הפקודה היא חלק אינטגרלי מפיתוח תוכניות ופתרון בעיות. לכן, חשוב לדעת כיצד לקרוא ולהתמודד עם פרמטרים שורת הפקודה בפרוגרמה שכתבנו.

## כיצד לעשות זאת

לחילופין מהרצת תוכנית בצורת UI, רוב התוכניות מאפשרות לנו להזין פרמטרים דרך שורת הפקודה. כדי לקרוא פרמטרים אלו בפרוגרמה היאכללנו, נשתמש בפונקציה מובנית קרוייה `getArgs` המכילה את כל הפרמטרים שהוזנו ומחזירה רשימה הכוללת את כל המחרוזות הפרמטרים.

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("הפרמטרים שהורדו: " ++ show args)
```

כאשר מריצים את התוכנית עם פרמטרים, נקבל את הפלט הבא:

```bash
runhaskell commandline.hs hello world
הפרמטרים שהורדו: ["hello","world"]
```

בכדי להפעיל תוכנית עם פרמטרים מבלי להריץ את הקוד, נוכל להשתמש בקוד שמוצג להלן:

```Haskell
import System.Environment
import Data.Char

main = do
    args <- getArgs
    let upperArgs = map (map toUpper) args
    putStrLn ("הפרמטרים באורך אותיות: " ++ show upperArgs)
```

בכדי להמיר את הפרמטרים לאותיות גדולות, השתמשנו בפונקציה `toUpper` מספר מוכלל שממירת את כל התווים לאותיות גדולות. נקבל את הפלט הבא כאשר מריצים את התוכנית עם הפרמטרים "hello" ו-"world":

```bash
runhaskell commandline.hs hello world
הפרמטרים באורך אותיות: ["HELLO","WORLD"]
```

## חיפוש מיקומו של פרמטר

ישנם מקרים שבהם נרצה לחפש האם פרמטר מסוים נמצא ברשימת הפרמטרים שה