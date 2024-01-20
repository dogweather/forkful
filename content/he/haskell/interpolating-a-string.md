---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# מאמר דרך ההשל: ריבוב מחרוזות בהשל (Haskell)

## מה ולמה?
ריבוב מחרוזות הוא תהליך בו נכנסים ערכים של משתנים מתוך הקוד לתוך מחרוזת. התכנתים עושים זאת כדי להדפיס, לבנות או למניפולציה של מחרוזות בצורה דינאמית.

## איך:
```haskell
import Text.Printf (printf)

main :: IO ()
main = do
    let name = "Avraham"
        age  = 34
    putStrLn $ printf "שלום, אני %s ואני בן %d" name age
```
הפלט:
```
שלום, אני Avraham ואני בן 34
```

## צלילה עמוקה
- קונטקסט היסטורי: השל, שנכתב בתחילה בשנת 1990, מכיל ספריות כמו "Text.Printf" להפוך את ריבוב המחרוזת לפשוט יותר.
- חלופות: ספריה כמו "Text.InterpolatedString.Perl6" מאפשרת ריבוי של מחרוזות בצורה המזכירה שפות תכנות אחרות כמו Perl או פייתון.
- פרטי ההתקנה: פונקציית printf משתמשת בפרמטרים וסימני פרמטר (%s, %d) כדי למקם את הערכים בטקסט.

## ראה גם
למידה נוספת על ריבוב מחרוזות בהשל:
- דוקומנטציה אופיציאלית של הספרייה "Text.Printf": https://hackage.haskell.org/package/base-4.14.1.0/docs/Text-Printf.html
- מאמר שנכתב על נושא זה בקומוניטי: https://schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/printf
- פוסט בנושא ב-Stack Overflow: https://stackoverflow.com/questions/14557994/what-is-the-haskell-way-of-formatting-strings-with-variables-in-them