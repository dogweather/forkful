---
title:                "כתיבת קובץ טקסט"
html_title:           "Haskell: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
למה אנשים ימלו לכתוב קובץ טקסט בהאסקל?
האסקל היא שפת תכנות פונקציונלית חזקה, איכותית ומהירה. לכתוב קובץ טקסט בהאסקל יכול להיות דרך טובה לתאר נתונים, ליצור דוחות או ליצור קבצי קוד לניתוח ושמירה לשימוש עתידי.

## How To

כדי לכתוב קובץ טקסט בהאסקל, נשתמש בפונקציית Haskell המכניסה נתונים לקובץ טקסט. לדוגמה, ניצור קובץ טקסט חדש ונכתוב בו "Hello, world!" כדי להתחיל נצטרך ליצור פרויקט בהאסקל עם שם נתונים שנתיים במקום ליצור לולאה.

```Haskell
main = do
    let fileName = "hello.txt"
    writeFile fileName "Hello, world!"
```

נוכל גם להשתמש בפונקציית `appendFile` כדי להוסיף נתונים לקובץ קיים במקום ליצור קובץ חדש. לדוגמה:

```Haskell
main = do
    let fileName = "hello.txt"
    appendFile fileName "Hello again!"
```

## Deep Dive

כאשר אנו משתמשים בפונקציות קריאה וכתיבה בהאסקל יש לזכור כי הן יצרות חיבור עם התחם של הקובץ. נתחיל עם פונקציית `writeFile` שניתן להשתמש בה כדי ליצור קובץ טקסט חדש ולכתוב אליו מידע. פונקציית `writeFile` מקבלת שני פרמטרים - מסלול לקובץ ותוכן שנרצה לכתוב. המסלול יכול להיות כל פסקה במערכת הקבצים הנוכחית.

למשל, אם אנחנו נעדיף ליצור קובץ טקסט חדש בתיקיית המסמכים שלנו, נשתמש בפונקציית `writeFile "Documents/hello.txt" "Hello, world!"`

## See Also

- קריאות נוספות מאתר האסקל הר