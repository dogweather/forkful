---
title:                "Haskell: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## למה

בעת שליחת תכניות חדשות או שינויים בתכנית קיימת, כתיבת בדיקות בודקת כי התוכנית עובדת כפי שצוין ואין בעיות או תקלות לפני שהתוכנית מושגת על ידי המשתמשים. זה מקנה ביטחון ומבטיח שהתוכנית תוכל להיות בשימוש מוצלח.

## איך לעשות זאת

הנה דוגמאות לכתיבת בדיקות בשפת Haskell

```haskell
-- מתוך נתון ארבעה מספרים, המצא את המספר הגדול ביותר

maxNumber :: Int -> Int -> Int -> Int -> Int
maxNumber a b c d = max (max a b) (max c d)

-- בדיקות

main = do
    putStrLn "Testing maxNumber..."
    test "#1" (maxNumber 1 2 3 4 == 4)
    test "#2" (maxNumber 4 3 2 1 == 4)
    test "#3" (maxNumber 2 1 4 3 == 4)
    test "#4" (maxNumber 1 3 2 4 == 4)

test :: String -> Bool -> IO ()
test name result = if result
                      then putStrLn (name ++ " passed!")
                      else putStrLn (name ++ " failed.")
```

פלט:

```
Testing maxNumber...
#1 passed!
#2 passed!
#3 passed!
#4 passed!
```

## להעמיק

כתיבת בדיקות קשה עבור מתכנתים רבים, אך זה מאוד חשוב כדי לוודא שהתוכנית עובדת ביעילות ובאמינות. ישנם כמה טכניקות שניתן להשתמש בהן כדי לכתוב בדיקות בצורה יעילה וחכמה. כמו כן, ישנם כלים וספריות זמינים כמו QuickCheck ו-Hspec שעוזרים לכתוב בדיקות באופן ידידותי ומתקדם.

## ראה גם

- [QuickCheck ספריית הבדיקות של Haskell](https://hackage.haskell.org/package/QuickCheck)
- [Hspec ספריית הבדיקות של Haskell](https://hspec.github.io)
- [הכרת מיומנויות בכתיבת בדיקות ב-Haskell](https://blog.ezyang.com/2010/05/the-very-model-of-a-modern-noetherian-induction)
- [מדריך לכתיבת בדיקות באמצעות QuickCheck](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html)