---
title:                "Haskell: כתיבת בדיקות"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## למה
נכתוב כמה משפטים על *למה* מישהו יכול להתעסק בכתיבת בדיקות.

כתיבת בדיקות היא חלק חשוב מתהליך הפיתוח בכל שפת תכנות, ובמיוחד ב-Haskell. בעזרת הבדיקות, אנו יכולים לוודא שהקוד שכתבנו עובד כפי שצריך ולזהות שגיאות לפני שהן מופעלות בסביבה ההפעלה האמיתית של התוכנית.

## איך לעשות זאת
לכתוב בדיקות ב-Haskell הוא פשוט כמו לכתוב קוד רגיל. נוכל להשתמש בספריית HUnit או QuickCheck כדי לכתוב ולהריץ את הבדיקות שלנו. הנה דוגמאות בלוק קוד המראות איך לממש בדיקות באמצעות HUnit ו-QuickCheck:

```Haskell
import Test.HUnit
import Test.QuickCheck

-- דוגמא לבדיקת יחידה באמצעות HUnit
-- בדיקה שהפונקציה add מקבלת שני מספרים ומחזירה את הסכום שלהם
testAdd :: Test
testAdd = TestCase (assertEqual "add 3 5 should be 8" 8 (add 3 5))

-- דוגמא לבדיקה מונחית באמצעות QuickCheck
-- בדיקה שהפונקציה reverse לא משנה את הסדר של האיברים ברשימה
prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

-- הרצת כל הבדיקות עם runTestTT ו-quickCheck
main :: IO ()
main = do
    runTestTT testAdd
    quickCheck prop_reverse
```

הפלט של הקוד הזה יוכיח שהפונקציות שלנו עובדות כפי שצריך:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++ GHCi: Test cases passed
+++ GHCi: Successfully generated & tested 100 test(s)..
+++ GHCi: SUCCESS (1 test(s) passed)
```

כמו שאתה רואה, כתיבת בדיקות היא פשוטה ונוחה ונותנת לנו הבטחה גדולה ביצירת תוכניות ברמה גבוהה יותר של אמינות ותקינות.

## מעמקים יותר
כשאנו כותבים בדיקות ב-Haskell, ישנם מספר פרטים קט