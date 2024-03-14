---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:17.593310-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D8\u05E1\u05D8\u05D9\u05DD \u05D1\
  \u05D4\u05E1\u05E7\u05DC \u05DE\u05D3\u05D5\u05D1\u05E8\u05EA \u05E2\u05DC \u05D5\
  \u05D9\u05D3\u05D5\u05D0 \u05E9\u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA\
  \ \u05E9\u05DC\u05DA \u05E4\u05D5\u05E2\u05DC\u05D5\u05EA \u05DB\u05E6\u05E4\u05D5\
  \u05D9 \u05D3\u05E8\u05DA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D0\u05D5\u05D8\
  \u05D5\u05DE\u05D8\u05D9\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05EA\
  \u05E4\u05D5\u05E1 \u05D1\u05D0\u05D2\u05D9\u05DD \u05DE\u05D5\u05E7\u05D3\u05DD\
  , \u05DC\u05D4\u05E7\u05DC \u05E2\u05DC \u05E9\u05D9\u05E0\u05D5\u05D9\u05D9 \u05DE\
  \u05D1\u05E0\u05D4 \u05D5\u05DC\u05EA\u05E2\u05D3 \u05D4\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.418942-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D8\u05E1\u05D8\u05D9\u05DD \u05D1\u05D4\
  \u05E1\u05E7\u05DC \u05DE\u05D3\u05D5\u05D1\u05E8\u05EA \u05E2\u05DC \u05D5\u05D9\
  \u05D3\u05D5\u05D0 \u05E9\u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA\
  \ \u05E9\u05DC\u05DA \u05E4\u05D5\u05E2\u05DC\u05D5\u05EA \u05DB\u05E6\u05E4\u05D5\
  \u05D9 \u05D3\u05E8\u05DA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D0\u05D5\u05D8\
  \u05D5\u05DE\u05D8\u05D9\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05EA\
  \u05E4\u05D5\u05E1 \u05D1\u05D0\u05D2\u05D9\u05DD \u05DE\u05D5\u05E7\u05D3\u05DD\
  , \u05DC\u05D4\u05E7\u05DC \u05E2\u05DC \u05E9\u05D9\u05E0\u05D5\u05D9\u05D9 \u05DE\
  \u05D1\u05E0\u05D4 \u05D5\u05DC\u05EA\u05E2\u05D3 \u05D4\u05EA\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת טסטים בהסקל מדוברת על וידוא שהפונקציות שלך פועלות כצפוי דרך בדיקות אוטומטיות. מתכנתים עושים זאת כדי לתפוס באגים מוקדם, להקל על שינויי מבנה ולתעד הת comport ומה behavior, ובכך להפוך את בסיס הקוד ליותר נשמר וקל לקנה מידה.

## איך לעשות זאת:

הסקל תומך במסגרות טסט שונות, אך שתיים פופולריות הן `Hspec` ו-`QuickCheck`. Hspec מאפשר לך להגדיר מפרטים קריאים לאדם עבור הקוד שלך, בעוד QuickCheck מאפשר לך לייצר טסטים אוטומטית על ידי תיאור תכונות שהקוד שלך צריך לעמוד בהן.

### שימוש ב-Hspec

ראשית, הוסף `hspec` אל קובץ תצורת כלי הבנייה שלך (למשל, `stack.yaml` או קובץ `cabal`). לאחר מכן, יבא את `Test.Hspec` וכתוב טסטים כמפרטים:

```haskell
-- file: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "מוסיף שני מספרים" $
    add 1 2 `shouldBe` 3

  it "מחזיר את המספר הראשון כאשר מוסיפים אפס" $
    add 5 0 `shouldBe` 5
```

לאחר מכן, הרץ את הטסטים באמצעות כלי הבנייה שלך, והתוצאה תיראה כך:

```
MyLib.add
  - מוסיף שני מספרים
  - מחזיר את המספר הראשון כאשר מוסיפים אפס

נגמר ב-0.0001 שניות
2 דוגמאות, 0 כשלונות
```

### שימוש ב-QuickCheck

עם QuickCheck, אתה מבטא תכונות שהפונקציות שלך צריכות לעמוד בהן. הוסף את `QuickCheck` לתצורת הפרויקט שלך, ואז יבא אותו:

```haskell
-- file: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

הרצת הטסטים הללו תייצר באופן אוטומטי קלטים כדי לבדוק את התכונות המצוינות:

```
+++ OK, עבר 100 טסטים.
+++ OK, עבר 100 טסטים.
```

בדוגמאות של Hspec ו-QuickCheck, סדרות הטסטים משמשות כתיעוד פעיל שיכול לאמת באופן אוטומטי את נכונות הקוד שלך.
