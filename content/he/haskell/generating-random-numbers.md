---
title:    "Haskell: יצירת מספרים אקראיים"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## למה

בעולם התכנות, פעולת ההגרלה (randomization) היא כלי חשוב ונפוץ. היא משמשת למגוון רב של מטרות, כגון בניית משחקים, יצירת תמונות מיוחדות, והגברת תחושת חשיבה יצירתית בתוכניות. בשפת Haskell, אפשר ליצור רצף מספרים אקראיים בקלות רבה ובמיוחד בעזרת ספריית `random`.

## כיצד לעשות זאת

הנה דוגמא פשוטה לפונקציה המייצרת מספרים אקראיים באמצעות הספרייה `random`:

```Haskell
import System.Random

-- פונקציה המחזירה מספר שלם אקראי בין 1 ל-10
randomInt :: IO Int
randomInt = randomRIO (1,10)
```

הפונקציה `randomInt` משתמשת בפעולת ההגרלה `randomRIO` שמקבלת כפרמטרים שני מספרים שמגדירים תחום המספרים האקראיים המשתנים עליו הפונקציה תפעל. על מנת להשתמש בפונקציה `randomInt`, נצטרך להיות תחילה בחבילת `System.Random` ולהשתמש במושג של מונד Hellermonde. פתחו תוכניית Haskell חדשה ונגיד שם פעם `randomInt` כדי להיעזר בנוסחאת המתרגם הנ"ל.

## למעמיקים

במקום להשתמש בפונקציות מובנות כמו `randomRIO`, ניתן לבנות מחולל מספרים אקראיים משלנו בעזרת המודול `System.Random`.

```Haskell
import System.Random

-- מחולל מספרים אקראיים עם זריקת המזל
random :: Int -> Int -> [Int]
random seed range = map (`mod` range) $ iterate (nextSeed seed) seed
  where
    nextSeed :: Int -> Int -> Int
    nextSeed seed = (3 * seed + 1) `mod` (2 ^ 31)

-- פונקציה המוחזרת מספר שלם אקראי בטווח מסוים
randomInt :: Int -> Int -> Int -> Int
randomInt seed range index = random seed range !! index
```

המחולל `random` משתמש בזריקת המזל (random seed) כדי לייצר ר