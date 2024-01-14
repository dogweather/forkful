---
title:                "Haskell: מחיקת תווים התואמים דפוס"
simple_title:         "מחיקת תווים התואמים דפוס"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

מחיקת תווים המתאימים לתבנית עלולה להיות שימושית במגוון מצבים, כדוגמת ניקוי נתונים סביב אותה תבנית או טיפול במחרוזת שמכילה תווים מיותרים.

## כיצד לעשות זאת

למחוק תווים המתאימים לתבנית בשפת Haskell ניתן להשתמש בפונקציית `filter`, המקבלת פונקציה תנאי ורשימה ומחזירה רשימה חדשה עם האיברים שעונים על התנאי. לדוגמה, נגדיר פונקציה שמקבלת מחרוזת ומחזירה את המחרוזת ללא תווים שגדולים מאו או קטנים מאו:

````Haskell
deleteChars :: String -> String
deleteChars str = filter (not . (\x -> x == 'a' || x == 'A')) str
````
כעת נבצע מספר בדיקות ונראה את הפלט:

````Haskell
deleteChars "Hello World!" -- "Hello World!"
deleteChars "aBcDeFgHi" -- "BcDeFgHi"
deleteChars "abcdefgh" -- "abcdefgh"
````

## טפס עמוק

בטפס זה ראינו כיצד ניתן למחוק תווים המתאימים לתבנית בשפת Haskell באמצעות פונקציית `filter`. ניתן להשתמש בפונקציות נוספות כגון `map` ו- `foldr` כדי לבצע פעולות נוספות ומתקדמות על המחרוזת. כמו כן, ניתן להשתמש בביטויים רגולריים כדי למחוק תווים לפי תבניות מתוחכמות יותר.

## ראו גם

- [מדריך מפורט על פונקציית `filter` ב-Haskell](https://www.haskell.org/tutorial/functions.html#filter)
- [מדריך לביטויים רגולריים ב-Haskell](http://blog.danieljanus.pl/2012/06/07/regular-expresions-in-haskell/)
- [מידע נוסף על טיפול במחרוזות בשפת Haskell](https://wiki.haskell.org/Strings)