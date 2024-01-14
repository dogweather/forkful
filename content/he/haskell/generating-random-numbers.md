---
title:                "Haskell: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# מדוע

מחשבים מייצרים מספרים אקראיים כדי להגביר את הסקרנות וליצור חווית משחק יותר מרתקת. פונקציית הגרילה של הסקיפט של הספריה של הסמף הוא דרך נפלאה ליצור מספרים אקראיים בכניסת אפליקציות עדיין לא משתמשות במזהה שערכים.

## איך לעשות

בכדי ליצור מספרים אקראיים בשפת הסקל, ניתן להשתמש בפונקציית הגרילה המעולה של הספריה של הסמף. ניתן להשתמש בקוד הבא כדי ליצור 10 מספרים אקראיים בטווח של 1 עד 100:

```Haskell
import System.Random

randomNums :: IO [Int]
randomNums = getStdRandom (randomRs (1,100))

main :: IO ()
main = do
  nums <- randomNums
  print nums
```

פלט:

```
[43,20,87,64,92,10,36,11,57,8]
```

## Deep Dive

כעת, נכנס לעומק ונבין את אלגוריתם הגרילה של הסמף. הפונקציה עובדת על ידי יצירת מספרים אקראיים במשתנה אחד, ומשחקת אותם עליהם עד לקבלת חיך במספר שונה. אלגוריתם זה מבטיח כי כל מספר קיבולתי יוצא עם אותה הנסיבה כמו כל מכשיר אחר.

מלבד זאת, ניתן לעשות שימוש בפונקציות נוספות של הסמף כמו `randomR` ו- `randomIO` כדי ליצור מספרים אקראיים בטווחים ודרכים שונות.

## ראה גם

- [הספריה של הסמף בדוקומנטציית Haskell](https://hackage.haskell.org/package/random)
- [מאמר מפורט על אלגוריתם הגרילה](https://en.wikipedia.org/wiki/Random_number_generation)