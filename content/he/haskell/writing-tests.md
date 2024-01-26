---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות היא תהליך שבו מפתחים מנסחים תרחישים לאימות התנהגות התוכנה. עושים את זה כדי לוודא שהקוד עובד כהלכה ולזהות באגים לפני השקתו.

## איך לעשות:
בהסקל ניתן לכתוב בדיקות באמצעות הספריה HUnit. דוגמא לקוד בדיקה:

```Haskell
import Test.HUnit

test1 :: Test
test1 = TestCase (assertEqual "Should add numbers" 4 (2 + 2))

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = runTestTT tests
```

הרצת קוד זה תביא את הפלט:
```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

## צלילה לעומק:
הסטוריה: בדיקות אוטומטיות התפתחו במקביל לשפות תכנות והן קיימות מאז שנות ה-50.
אלטרנטיבות: QuickCheck היא ספריה נוספת לבדיקות מבוססות תכונות.
פרטי יישום: ב-HUnit, `TestList` מאפשר להריץ רשימת בדיקות ו-`TestCase` מאפשר לבדוק טענה ספציפית.

## ראו גם:
- [HUnit User's Guide](http://hackage.haskell.org/package/HUnit-1.6.0.0/docs/Test-HUnit.html)
- [ההיסטוריה של בדיקות אוטומטיות](https://en.wikipedia.org/wiki/Test_automation#History)
