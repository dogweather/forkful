---
title:                "הפיכת מחרוזת לאותיות רישיות"
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מדובר בהפיכת כל אות לאות גדולה במחרוזת, פעולה ששימושית לייצור קונסיסטנטיות וסדר בטקסטים, כמו כותרות.

## איך לעשות:
```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize = map toUpper

main :: IO ()
main = putStrLn (capitalize "שלום עולם!")
-- Output: שלום עולם!
```

## צלילה עמוקה
במערכות כתב היד ההשוואתיות, תוכנות כמו ASCII או Unicode מאפשרות המרה של אותיות קטנות לגדולות. בהאסקל, פונקציית `toUpper` מהמודול `Data.Char` מבצעת זאת לכל תו. חשוב לזכור שכל תו מומר בנפרד ואין התחשבות במבנה המילה. בנוסף ל`map toUpper`, יש אלטרנטיבות כמו כתיבת פונקציה רקורסיבית משלך או השימוש ב-fold. כפונקציה מובנית, `toUpper` היא אופטימלית ומבוססת על טבלת התמרות סטנדרטית.

## ראה גם
- [Haskell `Data.Char` documentation](http://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html)
- [Unicode Character Database](https://www.unicode.org/ucd/)
- מדריך למתחילים בהאסקל: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)