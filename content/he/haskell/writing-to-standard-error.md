---
title:    "Haskell: כתיבה לתקליט שגיאה סטנדרטי"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

כתיבת הודעות לשגיאה תוך שימוש בHaskell היא כלי חשוב כאשר מתכנת רוצה להציג את הודעת השגיאה בצורה מפורטת ומובנת למשתמש. זה מאפשר למשתמש להבין מה מסובך את התוכנית ואיפה צריך להתחיל התיקון.

## איך לעשות זאת

שימוש בפונקציית `hPutStrLn` בספריית `System.IO` יאפשר לנו לכתוב את הודעת השגיאה לפלטר של שגיאות הסיסטם. לדוגמה:

```Haskell
main = do
    putStrLn "הקלד את המספר שלך:"
    input <- getLine
    let number = read input :: Int
    if number <= 0
        then hPutStrLn stderr "מספר לא חוקי נכנס!"
        else putStrLn $ "המספר שלך הוא: " ++ show number
```

פלט עבור קלט לא חוקי יהיה:

```
הקלד את המספר שלך:
-3
מספר לא חוקי נכנס!
```

פלט עבור קלט חוקי יהיה:

```
הקלד את המספר שלך:
12
המספר שלך הוא: 12
```

## טיפול עמוק

שימוש במכשיר הפלט של שגיאות הסיסטם יכול להיות שימושי כאשר רוצים לחזור על תהליך כל הפעולות שנעשו לפני קריסת התכנית. בנוסף, זה יכול להיות שימושי כאשר רוצים להציג את מיקום השגיאה בקוד כדי שיהיה קל יותר לתקן.

## ראה גם

- [נסלקה עמוקה ב-Haskell IO](https://wiki.haskell.org/Deep_inside_your_IO)
- [חלקיקים מהעגה: IO](https://mmhaskell.com/blogs/monads/monadic-io)
- [ממיר טבלאות IO](https://blog.feabhas.com/2017/10/c-using-casts-to-convert-an-i-o-table-project-to-freertos/)