---
title:                "כתיבה לקובץ טקסט"
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לקובץ טקסט זה פעולה שבה אנו יוצרים או מעדכנים קובץ במערכת הקבצים ומזינים לו טקסט. תכניתנים עושים זאת כדי לשמור נתונים, להעביר מידע בין תהליכים או ליומן (לוג) התרחשויות.

## איך לעשות:
```Haskell
-- פונקציה שכותבת טקסט לקובץ
writeToFile :: FilePath -> String -> IO ()
writeToFile path content = writeFile path content

-- דוגמה לשימוש בפונקציה
main :: IO ()
main = do
    let path = "hello.txt"
    let content = "שלום, עולם!"
    writeToFile path content
```

זה יצור או יעדכן את הקובץ `hello.txt` עם הטקסט "שלום, עולם!".

## עיון נוסף:
הפונקציה `writeFile` בHaskell כותבת טקסט לקובץ במרבית המערכות הפעלה. היא חלק מהסטנדרט של השפה מתקופת ה-90's. ישנם אלטרנטיבות כגון `appendFile` להוספת טקסט לקובץ קיים ו-streaming ספריות כמו `conduit` או `pipes` לעבודה עם קבצים גדולים. פרטי היישום של פונקציות אלו מסתמכים על מנגנון קלט/פלט (I/O) של הסביבה בה הקוד רץ.

## ראה גם:
- [Haskell IO tutorial](http://learnyouahaskell.com/input-and-output)
- [Haskell `writeFile` documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:writeFile)
- [Haskell `pipes` library](https://hackage.haskell.org/package/pipes)
- [Haskell `conduit` library](https://hackage.haskell.org/package/conduit)