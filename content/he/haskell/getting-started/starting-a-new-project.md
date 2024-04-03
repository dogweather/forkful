---
date: 2024-01-20 18:03:43.797601-07:00
description: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\
  \ \u05D7\u05D3\u05E9 \u05D1-Haskell \u05D4\u05D9\u05D0 \u05D9\u05E6\u05D9\u05E8\u05EA\
  \ \u05E1\u05D1\u05D9\u05D1\u05EA \u05E2\u05D1\u05D5\u05D3\u05D4 \u05D7\u05D3\u05E9\
  \u05D4 \u05DC\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA. \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05E1\u05D3\u05E8 \u05D0\u05EA \u05D4\u05E4\u05D9\u05EA\u05D5\
  \u05D7 \u05D5\u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05DE\u05D1\u05E0\u05D4 \u05EA\
  \u05DE\u05D9\u05DB\u05D4 \u05E0\u05DB\u05D5\u05DF \u05DE\u05D4\u05D4\u05EA\u05D7\
  \u05DC\u05D4."
lastmod: '2024-03-13T22:44:39.414318-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9 \u05D1-Haskell \u05D4\u05D9\u05D0 \u05D9\u05E6\u05D9\u05E8\u05EA \u05E1\
  \u05D1\u05D9\u05D1\u05EA \u05E2\u05D1\u05D5\u05D3\u05D4 \u05D7\u05D3\u05E9\u05D4\
  \ \u05DC\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA."
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
weight: 1
---

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
