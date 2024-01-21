---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:56:08.717306-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/reading-command-line-arguments.md"
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