---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:57.162560-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים היא פעולה שבה מחוללים ערכים שאינם ניתנים לחיזוי מראש. תוכניתנים עושים זאת כדי לבצע בדיקות, להוסיף גיוון לאפליקציות או לסייע באלגוריתמים שדורשים אלמנט של אקראיות.

## איך לעשות:
בהסקל, תפקיד שימוש במחוללי מספרים אקראיים דורש קצת הגדרה. נתחיל עם סימפליות:

```Haskell
import System.Random (randomRIO)

simpleRandom :: IO Int
simpleRandom = randomRIO (1, 100)

main :: IO ()
main = do
  num <- simpleRandom
  print num
```

כאשר תריץ את הקוד הזה, `simpleRandom` יחזיר מספר אקראי בין 1 ל-100.

## צלילה עמוקה
במקור, הסטנדרט של הסקל ליצירת מספרים אקראיים היה פשוט, אבל הוא מתמודד עם בעיות של ביצועים וחוסר אחידות. כדי לשפר זאת, מציע הסטנדרט החדש (`random-1.2`) ממשק משתמש חדש וכלים מתקדמים יותר ליצירת זרמים של מספרים אקראיים.

צורה אחרת ליצירת אקראיות היא שימוש במחולל פסוודו-אקראי (PRNG - Pseudo Random Number Generator). PRNG זה יוצר זרם של מספרים שנראה אקראי אבל הוא בעצם נגזר מערך התחלתי קבוע (seed). הנה דוגמה:

```Haskell
import System.Random (newStdGen, randomRs)

pseudoRandomNumbers :: Int -> IO [Int]
pseudoRandomNumbers seed = do
  gen <- newStdGen
  return $ take seed $ randomRs (1, 100) gen

main :: IO ()
main = do
  nums <- pseudoRandomNumbers 5
  print nums  -- ידפיס רשימה של 5 מספרים אקראיים
```

בסיסיות כמו `randomRIO` גם הם מהווים עטיפה סביב מנגנוני PRNG.

## ראה גם
- [System.Random מודול](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html): המקור הראשי ליצירת מספרים אקראיים בהסקל.
- [Real World Haskell קבוצה הקדושה ואקראיות](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html): פרק בספר המתמקד בבדיקות איכות ואיך מספרים אקראיים משחקים תפקיד בתהליך זה.
- [SO שאלות ותשובות](https://stackoverflow.com/questions/tagged/haskell+random): פורום שאלות ותשובות על יצירת מספרים אקראיים בהסקל.
