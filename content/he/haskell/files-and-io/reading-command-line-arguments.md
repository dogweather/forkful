---
aliases:
- /he/haskell/reading-command-line-arguments/
date: 2024-01-20 17:56:08.717306-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05EA\
  \u05D5\u05DB\u05E0\u05D4 \u05DE\u05E7\u05D1\u05DC\u05EA \u05E7\u05DC\u05D8 \u05DE\
  \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D3\u05E8\u05DA \u05D4\u05D8\u05E8\u05DE\
  \u05D9\u05E0\u05DC. \u05EA\u05DB\u05E0\u05D9\u05EA\u05D0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E2\u05D1\
  \u05D9\u05E8 \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD \u05D3\u05D9\u05E0\u05DE\
  \u05D9\u05D9\u05DD \u05DC\u05EA\u05D5\u05DB\u05E0\u05D4 \u05D1\u05E2\u05EA \u05D4\
  \u05E8\u05E6\u05D4."
lastmod: 2024-02-18 23:08:52.903687
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05EA\
  \u05D5\u05DB\u05E0\u05D4 \u05DE\u05E7\u05D1\u05DC\u05EA \u05E7\u05DC\u05D8 \u05DE\
  \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D3\u05E8\u05DA \u05D4\u05D8\u05E8\u05DE\
  \u05D9\u05E0\u05DC. \u05EA\u05DB\u05E0\u05D9\u05EA\u05D0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E2\u05D1\
  \u05D9\u05E8 \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD \u05D3\u05D9\u05E0\u05DE\
  \u05D9\u05D9\u05DD \u05DC\u05EA\u05D5\u05DB\u05E0\u05D4 \u05D1\u05E2\u05EA \u05D4\
  \u05E8\u05E6\u05D4."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא התהליך שבו תוכנה מקבלת קלט מהמשתמש דרך הטרמינל. תכניתאים עושים זאת כדי להעביר פרמטרים דינמיים לתוכנה בעת הרצה.

## איך לעשות:
ב-Haskell, קוד לקריאה משורת הפקודה פשוט וקצר. נשתמש במודול `System.Environment`:

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print args
```

הרצת התוכנית עם הפרמטרים `hello` ו-`world` תביא את הפלט:

```
["hello", "world"]
```

ניתן גם לקרוא ארגומנט אחד עם `getProgName` לשם התוכנית ו-`getArgs` לשאר הפרמטרים.

## צלילה לעומק
בעבר, אירועים כמו תחרות ה-ICFP ופורומים קהילתיים עזרו לפתח פונקציונליות בסיסית זו. ישנם אלטרנטיבות כגון מודולים חיצוניים כמו `optparse-applicative` לפרסינג אופציות מתקדמות יותר. ברמת המימוש, `getArgs` ו-`getProgName` מומשו בחלקם ב-C על מנת לאפשר גישה נוחה למשתני הסביבה של מערכת ההפעלה.

## ראה גם
- [Haskell System.Environment documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)
- [optparse-applicative on Hackage](https://hackage.haskell.org/package/optparse-applicative)
- [Haskell Wiki book on command line parsing](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/IO)
- פוסט בבלוג אודות קריאת ארגומנטים ופרסינג ב-Haskell (חיפוש בגוגל עם מפתחי המילים "Haskell command line arguments blog" יניב אפשרויות)
