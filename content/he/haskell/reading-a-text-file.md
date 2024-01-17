---
title:                "קריאה של קובץ טקסט"
html_title:           "Haskell: קריאה של קובץ טקסט"
simple_title:         "קריאה של קובץ טקסט"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט היא תהליך בו מתבצעת קריאה שורה אחר שורה של טקסט שמוכנס בקובץ מיוחד. תהליך זה מאפשר למתכנתים לקרוא, לערוך ולהתמודד עם קלט של משתמשים ואירועים חיצוניים.

## איך לעשות זאת:
לאחר התקנת השפה Haskell, ניתן להשתמש בפונקציה "getContents" כדי לקרוא את הקלט הטקסטואלי של המשתמש בצורה נוחה וקלה. בהמשך ניתן להשתמש בפונקציות נוספות כמו "readFile" לקרוא קבצים בצורה יעילה יותר. הנה דוגמאות של קוד ותוצאות:

```haskell
getContents
-- למשל, אם הקלט שהמשתמש מכניס הוא "Hello World!", התוכנית תדפיס: "Hello World!"

readFile "numbers.txt"
-- אם הקובץ numbers.txt מכיל את הטקסט "1\n2\n3\n4\n5", התוצאה תהיה: "12345"
```

## עיון מעמיק:
לפני יצירת השפה Haskell, בשנת 1990, זאפ מתחוללת הוצע כפתרון לקריאת קבצים טקסטואליים. השיטה הזו מאפשרת לשלוט על הקלט לפי צורת יצירתו ולטפל בו בצורה מאורגנת ויעילה יותר.

ייתכן שחלק מהקוראים יכירו במשפט "open()", שניתן להשתמש בו כדי לקרוא ולכתוב קבצים בשפות כמו C ו-Python.

כעת, עם הגעתו של Haskell, ניתן לנהל קובץ טקסט בצורה פשטנית ומאורגנת באמצעות פונקציות כמו "getContents" ו-"readFile".

## ראו גם:
למידע נוסף על כיצד לקרוא קבצים בשפת Haskell, ניתן לקרוא את המאמר "List of IO Functions in Haskell" באתר "Haskellers".
למידע נוסף על זא לחץ כאן

https://wiki.haskell.org/Handling_Files