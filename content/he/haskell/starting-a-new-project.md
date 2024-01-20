---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?

התחלת פרויקט חדש בהסקל היא תהליך שבו אנו מתכנתים את הפונקציונליות הבסיסית של האפליקציה שלנו. אנחנו עושים זאת כדי ליישב את בסיס הפרויקט שלנו, ולכן להקל על פיתוח משני ופחות ממוקד.

## כיצד ל-:

אתחל את הפרויקט שלך עם הפקודה הבאה:

```Haskell
stack new my-project
```

הכנס לתיקיית הפרויקט והתחל לכתוב קוד עם הפקודות האלה:

```Haskell
cd my-project
stack setup
stack ghci
module Main where

main :: IO ()
main = putStrLn "Hello, World!"
```

כאשר אתה מריץ את הפקודה הזאת, ההודעה "Hello, World!" תודפס:

```Haskell
stack run
```

## הצצה מעמיקה

היסטורית, פיתוח פרויקטים בהסקל הבסיסי המהיר (Quickstarting projects with Haskell) היה מורכב. אף שהשפה די לא מרובה, המערך שחייב להכון היה משאית. "Stack" אשר התנהל לא מזמן, הקל על התהליך משמעותית.

אלטרנטיבות נוספות ל- Stack כוללות Cabal ו- GHC המתקנים את הסביבה שלך ומספקים לך כלים לניהול התלויות שלך, אם כי Stack הוא המשובח ביותר לחלבה עם הסקל החדש.

ההתקנה הראשונית שלך תמשוך את GHC (התרגיל בהסקל), עם ספריות נלוות, אל המחשב שלך. זהו התהליך שמכונה `stack setup`.

## ראה גם

* [מסמכים של Stack](https://docs.haskellstack.org/en/stable/README/)
* [הכוונה ראשונית להסקל](http://learnyouahaskell.com/chapters)
* [Cabal](https://www.haskell.org/cabal/users-guide/quick-start.html)
* [GHC](https://www.haskell.org/ghc/)