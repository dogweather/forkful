---
title:                "הדפסת פלט נקודת תיקון"
html_title:           "Haskell: הדפסת פלט נקודת תיקון"
simple_title:         "הדפסת פלט נקודת תיקון"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

כתיבת פלט דיבאג בפונקציות ב הסקל - טון לא יפוקס ובסגנון לא פורמלי

## מה ולמה?

תעודת הוצאה דיבוג היא כאשר מתכנתים מדפיסים פלט מידע לצורך ניתוח בעת מפתחת כוללות כגון מצב התוכנית, ערך המשתנים, וכו '. היא שיטה נפוצה למציאת באגים ושיפור ביצועים של קוד. 

## איך לעשות:

דוגמאות קוד ופלט להלן בלוקים ```Haskell ...```

קוד:
```Haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
  putStrLn "Enter a number: "
  n <- readLn
  let result = factorial n
  putStrLn ("The factorial of " ++ show n ++ " is " ++ show result)
  putStrLn "Debug output:"
  print [n-2, n-1]
```

פלט:
```
Enter a number:
5
The factorial of 5 is 120.
Debug output:
[3,4]
```

## העמקה:

### היסטוריה:
תעודת הוצאה דיבוג הייתה נפוצה כאשר בתורה פותחה בשנות ה 70 כאשר טכנולוגיות פיתוח כדי לקבוע את נקודות השגיאה. מאז, זה הפך מנפוצה יותר כאשר טכנולוגיות מתוקשבות השתמשו בתעודת הוצאה דיבוג כמיוחד בתכנות קצר. 


### אלטרנטיבות:
ישנם כמה אלטרנטיבות לתעודת הוצאה דיבוג, כמו שימוש במתכנתי שאילתות להתאמת תנאים ומבחן יחידות (באנגולית: unit testing). כמו כן, יש כלים ספציפיים לתעודת הוצאה דיבוג כגון (באנגולית: debuggers) שמשמשים לניתוח כולל, אבל הם לא מתאימים לכל הפשע בגלל עלויות נקיון הקוד.

### פירטי עיבוד:
בדרך כלל, תעודת הוצאה דיבוג יתווספו את הפן האדם מלהשלים את הקוד. כמו כן, באמצעות כלי תעודת הוצאה דיבוג מתבצע בזמן אמת, זה אנן אפטים לפשע אחר-תהליך מתוך תוקן הקוד.

## ראו גם:

- [The Power of Debugging in Haskell](https://www.schoolofhaskell.com/user/commercial/content/the-power-of-debugging-in-haskell)
- [Debugging Tips for Haskell](https://www.reddit.com/r/haskell/comments/1g39jr/debugging_tips_for_haskell/)
- [Haskell Debug Adapter for Visual Studio Code](https://github.com/JustusAdam/vscode-haskell-debug)