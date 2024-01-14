---
title:    "Haskell: קריאת ארגומנטים משורת פקודה"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה
למה אדם צריך לקרוא ארגומנטים מפקודת השורת פקודה? ייתכן שכדי לפתור בעיה המחייבת קלט מהמשתמש או כדי להתאים את פירוט התוכנית לטענת המשתמש.

## כיצד
```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("מספר הארגומנטים שהוקלדו הוא: " ++ show (length args))
```
כדי לקרוא ארגומנטים מפקודת השורת פקודה ב-Haskell ניתן להשתמש ב- `System.Environment` ולהוריד את הפונקציה `getArgs`. הפונקציה תחזיר את הארגומנטים כרשימה של מחרוזות. בקוד הדוגמה נציג את מספר הארגומנטים שהוקלדו על ידי ניצול `length` ו- `show`.

כאשר מריצים את הקוד עם הפרמטרים הבאים: `ghc reading-arguments.hs` נקבל תוכן הארכיטקטורה הבאה:

```shell
$ ./reading-arguments foo bar baz
מספר הארגומנטים שהוקלדו הוא: 3
```

## טיפול עמק
כדי לתפוס את הטיפול הכללי במבני נתונים, אנו כעת רק מתייחסים לקריאה של נתיב מול מופע מסוים. אם מדובר במופע מסתובב זה יכול לתת תמיכה למספר טיפולים כגון בחירה מתמש לפי דף התוכנית, אז זה יכול להיות דרך יעילה יותר לקידום פירוט התוכנית שלך.

## ראו גם
* [נתיב למדריך מתקדם ל-Haskell](https://www.haskell.org/tutorial/)
* [מכירת משמעות באמצעות התג ה- `getArgs`](https://www.haskell.org/hoogle/?hoogle=getArgs)
* [חוק השורת פקודה שנקרא על גבי shell](https://www.haskell.org/shell/)