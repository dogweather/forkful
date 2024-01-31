---
title:                "שימוש בדיבאגר"
date:                  2024-01-26T04:09:49.637027-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בדיבאגר"

category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/using-a-debugger.md"
---

{{< edit_this_page >}}

## מה ולמה?
שימוש במנפה תקלות משמעו צלילה לתוך הקוד שלך עם כלים שנועדו לבדוק, להשהות ולשנות תוכנית באמצע הרצה. מתכנתים עושים זאת כדי לרדוף אחרי באגים, להבין את זרימת התוכנית ולוודא שהקוד שלהם עושה בדיוק מה שהם מצפים שיעשה.

## איך לעשות:
בואו נצעד עם GHCi, הסביבה האינטראקטיבית של Haskell שיכולה לשמש כמנפה תקלות בסיסי. אתם מפעילים אותה עם הקוד שלכם ב-Haskell ומתחילים לחקור. הנה דוגמה:

```Haskell
main :: IO ()
main = do
    putStrLn "היי, מה השם שלך?"
    name <- getLine
    putStrLn $ "שלום, " ++ name ++ "! בואו ננפה."
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- נא לדמיין שיש כאן באג
```

להתחיל בניפוי עם GHCi:

```bash
$ ghci YourHaskellFile.hs
```

הגדרת נקודת עצירה ב-`buggyFunction`:

```Haskell
Prelude> :break buggyFunction
```

הרצת התוכנית שלך:

```Haskell
Prelude> :main
היי, מה השם שלך?
```

התוכנית שלך משהית ב-`buggyFunction`. כעת תוכלו לבדוק משתנים, לצעוד דרך הקוד ולהעריך ביטויים.

## צלילה עמוקה:
בהיסטוריה, המוניטין של Haskell בגינוי פונקציות טהורות וטיפוסיות חזקה הוביל לאמונה שכלים לניפוי תקלות הם פחות קריטיים. המציאות שונה - תוכניות מורכבות תמיד נהנות מכלים לניפוי תקלות טובים. GHCi מספקת פקודות לניפוי תקלות בסיסיות. עם זאת, לחוויה חזותית יותר או ליישומים בקנה מידה גדול יותר, ייתכן ותחפשו IDEs עם מנפי תקלות משולבים, כמו Visual Studio Code עם הרחבות Haskell או תוסף ה-Haskell של IntelliJ.

חלופות למנפה תקלות כוללות שימוש בהדפסות, הידועות כ-"ניפוי עם printf", או שימוש במערכת הטיפוסים החזקה של Haskell כדי להפוך מצבים שגויים לבלתי ניתנים לייצוג. עם זאת, לפעמים אין תחליף לצעידה דרך הקוד.

לגבי פרטי יישום, מנפה התקלות של Haskell עובד עם מערכת הזמן הרץ. הוא יכול לטפל בנקודות עצירה, בצעידת הביצוע ולאפשר בדיקת משתנים. עם זאת, מכיוון ש-Haskell מתבצע באופן עצלני, ייתכן והדברים יהיו קצת לא אינטואיטיביים. לנפות תוכנית Haskell לעיתים קרובות אומר לשמור עין על מתי ואיך ביטויים מתבצעים.

## ראו גם:
- [מדריך המשתמש של GHC - מנפה תקלות](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [תוסף ה-Haskell של IntelliJ](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
