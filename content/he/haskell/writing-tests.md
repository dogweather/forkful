---
title:                "כותבים ניסויים -  תרגום נכון צריך להיות: כתיבת ניסויים"
html_title:           "Haskell: כותבים ניסויים -  תרגום נכון צריך להיות: כתיבת ניסויים"
simple_title:         "כותבים ניסויים -  תרגום נכון צריך להיות: כתיבת ניסויים"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות היא תהליך שבו מתכנתים בודקים את תכניותיהם כדי לוודא שהן פועלות כמתוכננות. זה נעשה על מנת למנוע באגים ותקלות בתוכניות המוצגות למשתמשים בסופו של דבר. כתיבת בדיקות היא חלק חשוב מכתיבת קוד ומגבירה את אמינות התוכנית.

## איך לעשות?
בדיקות נכתבות בקוד ה- Haskell שלנו באמצעות פונקציות קודמות, מקובל "ערכים צפופים" וטסטים בודדים לפונקציות שחלק מהן. כל פונקציה צריכה להיות בדוקה לפני שהיא נכתבת כדי לוודא שהיא מחזירה את הערך המתאים ומביאה לאף באגים.

```haskell
add :: Int -> Int -> Int
add x y = x + y

shouldBeFive :: Bool
shouldBeFive = add 2 3 == 5

main :: IO ()
main = do
  if shouldBeFive
     then putStrLn "Test successful!"
     else putStrLn "Test failed..."
```

## המעמד העמוק
בתחילה, כתיבת בדיקות נעשתה ידנית על ידי מתכנתים. מאוחר יותר, כלים וספריות נוצרו כדי לסייע למתכנתים בתהליך. כלים כמו "HSpec" ו"Quickcheck" מפשטים את תהליך הבדיקה ומאפשרים לנו לבדוק יותר טוב את הקוד שלנו.

בנוסף, ישנן אלטרנטיבות לכתיבת בדיקות ברוב השפות התכנות, כגון מרכיבים אוטומטיים כמו "Travis CI" ו"Jenkins". בניגוד, הכתיבה של בדיקות ב- Haskell מאפשרת לנו שליטה יותר גדולה על התהליך ומבטיחה שהקוד שלנו יהיה תקין כל עת.

כל פונקציה צריכה להיות מבודקת באמצעות הבדיקות שכתבנו כדי לוודא שהיא מציעה את ההתנהגות הנכונה כשקלטים שונים נתונים לה.

## ראו גם
למידת Haskell בעזרת בדיקות: https://www.tutorialspoint.com/haskell/haskell_test.htm

דוגמאות בדיקות ב- Haskell: https://www.fpcomplete.com/blog/2017/04/writing-haskell-tests

מאמר על חשיבות הבדיקות בתהליך הפיתוח: https://hackernoon.com/why-testing-insider-tips-on-writing-sensibly-future-proof-code-c02f5cd2e462