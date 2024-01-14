---
title:                "Haskell: כותבים קובץ טקסט"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה?

כתיבת קובץ טקסט היא חלק חשוב ביישום שפת תכנות Haskell. בזכות הקוד פשוט ויעילות של Haskell, כתיבת קבצים טקסט עושה שימוש מאוד נפוץ וקל יותר להחשבה מורכבת. אנשים מתאימים לכתיבת קובץ טקסט כדי לשמור נתונים כמו קוד מקור או תמונות.

## איך לעשות

ההתחלה היא לכתוב הגדרה פשוטה של קובץ טקסט, באמצעות פונקציות פשוטות כמו "writeFile" ו"readFile". הנה דוגמה לכיצד לכתוב קובץ טקסט ב-Haskell:

```Haskell
main = do
    writeFile "myFile.txt" "זהו קובץ טקסט לדוגמה"
```

ונה כיצד נראה הפלט של התכנית:

```
זהו קובץ טקסט לדוגמה
```

כעת, ניתן לקרוא את הקובץ טקסט כדי לראות את המידע שנשמר בו:

```Haskell
main = do
    putStrLn "ראו את התוכן של הקובץ:"
    contents <- readFile "myFile.txt"
    putStr contents
```

והנה הפלט של התכנית:

```
ראו את התוכן של הקובץ:
זהו קובץ טקסט לדוגמה
```

## העיון העמוק

קריאת וכתיבת קבצים טקסט הוא פשוט רק חלק מהיכולות הרבות שהשפה Haskell יכולה להציע. ניתן להשתמש בפונקציות נוספות כמו "appendFile" להוספת תוכן לקובץ קיים, או "withFile" לביצוע פעולות מתקדמות כמו קורא רשת.

## ראו גם

- [מדריך לכתיבת קבצים ב-Haskell](https://wiki.haskell.org/Writing_a_file)
- [פונקציות חשובות לכתיבת קבצים ב-Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [למדו עוד על שפת התכנות Haskell](https://www.haskell.org/documentation/)