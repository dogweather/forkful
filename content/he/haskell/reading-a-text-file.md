---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# הכתיבה ב- Haskell: קריאת קובץ טקסט

## מה ולמה?

קריאת קובץ טקסט היא פעולה שבה תוכנה קוראת נתונים מתוך קובץ טקסט. תיכנים עושים זאת על מנת לעבד מידע, לשמור משתנים, לבנות דטה-בייסים ועוד.

## איך לעשות:

קוד ב-Haskell לקריאת קובץ:

```Haskell
import System.IO

main = do
    text <- readFile "test.txt"
    putStrLn text
```

אם "test.txt" מכיל את הטקסט "שלום עולם", הרצת הקוד תחזיר:

```Haskell
שלום עולם
```

## צלילה עמוקה

במרוצת השנים, Haskell פיתחה מספר דרכים לקריאת קבצים. התחלתית, הוספנו מודולים כמו `System.IO` לשם קריאה וכתיבה בקבצים. לאחר מכן, הוספנו פונקציונליות לטיפול בשגיאות שקורות במהלך הקריאה.

חלופות לקריאת קבצים ב-Haskell כוללות את המודולים `Data.ByteString` ו- `Data.Text`. כל אחד מהם משמש לטיפולים שונים של טקסט, ונבחר בהתאם לדרישות המצב.

## ראה גם:

1. [Haskell Wiki - עבודה עם קבצים](https://wiki.haskell.org/Working_with_files)
2. [Real World Haskell - קריאת וכתיבת קבצים](http://book.realworldhaskell.org/read/io.html)
3. [Hoogle - System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
4. [Stack Overflow - דיון בנושא קריאת קבצים ב-Haskell](https://stackoverflow.com/questions/7860351/reading-from-files-in-haskell)