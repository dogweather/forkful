---
title:                "התחלת פרויקט חדש"
aliases:
- /he/haskell/starting-a-new-project/
date:                  2024-01-20T18:03:43.797601-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
התחלת פרויקט חדש ב-Haskell היא יצירת סביבת עבודה חדשה לקוד שלך. תוכניתנים עושים זאת כדי לסדר את הפיתוח ולהבטיח מבנה תמיכה נכון מההתחלה.

## איך לעשות:
כדי להתחיל פרויקט חדש ב-Haskell, נעשה שימוש בכלים כמו `stack` או `cabal`. הנה דוגמא קצרצרה עם `stack`:

```Haskell
-- יצירת פרויקט חדש עם stack
stack new myProject

-- נכנסים לתיקייה שנוצרה
cd myProject

-- חיבור הפרויקט ל-REPL לבדיקות מהירות
stack ghci

-- מפעילים את הקוד
stack build
stack exec myProject-exe
```

פלט דוגמא:
```
Hello, Haskell!
```

## עיון מעמיק:
התחלת פרויקט ב-Haskell לא הייתה תמיד כזו פשוטה. בעבר, התכנתנים היו צריכים להגדיר הכל מאפס. כלים כמו `stack` ו-`cabal` הקלו על התהליך על ידי אוטומציה של יצירת מבנה פרויקט ותלותיו.
`Cabal` היה קודם ל`stack`, אבל עדיין נמצא בשימוש נרחב. `Stack` הוא חדש יותר, עם נטייה להיות פשוט יותר לשימוש ומציע ממשק ידידותי יותר למתחילים.
לכל אחד יתרונות וחסרונות, אבל שניהם שואפים לפשט את התהליך ולעודד התקנה קונסיסטנטית של תלותי חבילות.

## ראה גם:
- [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- [Cabal User Guide](https://www.haskell.org/cabal/users-guide/)
- [Haskell Getting Started](https://www.haskell.org/downloads/)
- [Haskell Package Hackage](https://hackage.haskell.org/)
