---
title:    "Haskell: קריאת ארגומנטים של שורת פקודה"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# למה

למה לקרוא את משתני השורה הפקודה של האסקל?

קריאת משתני השורה הפקודה היא מנגנון חשוב כאשר מדובר בשליטה על לוגיקה ופעולות של תוכניות האסקל. היא מאפשרת למשתמש להעביר פרמטרים כקלט בזמן ריצה ולהתאים את התוכנה לצרכיו המיוחדים ביותר.

# איך לעשות זאת

באמצעות התכונה System.Environment.getArgs, ניתן לקבל את המשתנים של השורה הפקודה כקלט ולעבד אותם בתוכניות האסקל. לדוגמה:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "המשתנים השורה הפקודה הם: " ++ show args
```

פלט:
המשתנים של השורה הפקודה הם: ["ארגומנט1", "ארגומנט2", "ארגומנט3"]

ניתן גם לעבד את המשתנים של השורה הפקודה באופן אישי כדי לקבל פרמטרים שונים. לדוגמה:

```Haskell
import System.Environment

main = do
    args <- getArgs
    case args of
        [num1, num2] -> do
            let result = (read num1 :: Int) + (read num2 :: Int)
            putStrLn $ "תוצאה: " ++ show result
        _ -> putStrLn "אין מספיק ארגומנטים."
```

פלט:
$ הרץ 5 10
תוצאה: 15

# צלילה עמוקה

כאשר מדובר בקריאת משתני השורה הפקודה, ישנם עוד מספר תכונות נוספות שניתן להשתמש בהן. לדוגמה, ניתן להשתמש ב- getProgName כדי לקבל את שם התוכנית הנוכחי. ניתן גם להשתמש בפונקציות כמו getArgsAndInitialize ו- withArgs עבור תכניות הגרפיקה שאינן משתמשות בפקודת קו התפריט.

# ראה גם

- [Haskell Wiki: פקודת קו התפריט](https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling)
- [מדריך שימושי לקריאת קבצי תוויות באסקל](https://docs.haskellstack.org/en/st