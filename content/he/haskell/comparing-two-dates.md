---
title:                "השוואת שתי תאריכים"
html_title:           "Haskell: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

כתיבת פונקציות בשפת התכנות Haskell נפוצה בעידן המודרני של התכנות, מאפשרת מבנים יעילים וקריאים ליצירת קוד מאורגן. תמיד יש תחום שמתמקד, ועכשיו אנחנו נתמקד בהשוואת שתי תאריכים בפונקציות Haskell ולמה זה חשוב למתכנתים.

## מה ולמה?

השוואת שתי תאריכים היא תהליך שבו משווהים בפורמט של תאריך את ההבדל בין שני תאריכים. תהליך זה מאפשר למתכנתים לבדוק אם תאריך מסוים מתאים לתנאים מסוימים, כגון תאריך תקין או תאריך מתאים יותר להשוואה.

## איך לעשות?

לדוגמה, נציג כמה קטעי קוד המדגימים איך לבצע השוואה בין שני תאריכים בפונקציות Haskell.

```Haskell
-- הגדרת הפונקציה הראשונה, עם פרמטרים שניים המייצגים שני תאריכים
compareDates :: Integer -> Integer -> Ordering
-- פונקציה המוחזרת מובאתת של השוואת שני תאריכים בעזרת פונקציות המובנות `compare` ו-`fromGregorian`
compareDates date1 date2 = compare (fromGregorian date1 1 1) (fromGregorian date2 1 1)
-- ערך המוחזר הוא מכיל את התוצאה של השוואה, מייצג באמצעות ערך המכיל את האפשרויות 'LT', 'EQ' או 'GT'

-- פונקציה שנייה, המשווה בין שלושה תאריכים בעזרת פונקציות המובנות `diffDays` ו-`fromGregorian`
compareThreeDates :: Integer -> Integer -> Integer -> Ordering
compareThreeDates date1 date2 date3 = compare (diffDays (fromGregorian date1 1 1) (fromGregorian date2 1 1)) (diffDays (fromGregorian date1 1 1) (fromGregorian date3 1 1))
-- ערך המוחזר מייצג האם התאריך הראשון נמצא בין התאריכים השני והשלישי, לפי התחלואה של התאריך המקורי.

compareDates 1997 1998 -- אם התאריכים שווים זה לזה, יוחזר EQ

compareThreeDates 1996 1998 2000 -- אם התאריכים נמצאים בסדר שנכון (1998 מתאים לבין 1996 ו2000), יוחזר LT

compareThreeDates 2002 1984 2013 -- אם התאריכים הראשון והשלישי לא נמצאים בין התאריך השני, יוחזר GT
```

## לעומק

כדי להשוות שני תאריכים בפונקציות Haskell, ניתן להשתמש בפונקציה `compare` המשווה בין שני ערכים על ידי השוואת ערכים מספריים. תוצאת השוואה היא בין הערכים LT, EQ או GT, המייצגים התאמה לתנאים שנקבעו. ניתן להשתמש גם בפונקציות נוספות כמו `diffDays`, המשווה בין שני תאריכים לפי היום המסוים בחודש.

קיימות גם אלטרנטיבות להשוואת שתי תאריכים בפונקציות Haskell,