---
date: 2024-01-26 00:56:23.122200-07:00
description: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA \u05E7\u05E9\u05D5\u05E8 \u05DC\u05E0\u05D9\
  \u05D4\u05D5\u05DC \u05D4\u05DC\u05D0 \u05E6\u05E4\u05D5\u05D9\u2014\u05D3\u05D1\
  \u05E8\u05D9\u05DD \u05E9\u05E2\u05DC\u05D5\u05DC\u05D9\u05DD \u05DC\u05D4\u05E9\
  \u05EA\u05D1\u05E9. \u05EA\u05D5\u05DB\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0\
  \ \u05DB\u05D9 \u05D4\u05EA\u05D5\u05DB\u05E0\u05D5\u05EA \u05E9\u05DC\u05D4\u05DD\
  \ \u05D9\u05D5\u05DB\u05DC\u05D5 \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\
  \u05DD \u05DE\u05E6\u05D1\u05D9\u05DD \u05D0\u05DC\u05D5 \u05D1\u05D7\u05DF, \u05D1\
  \u05DC\u05D9 \u05DC\u05E7\u05E8\u05D5\u05E1 \u05D0\u05D5\u2026"
lastmod: '2024-02-25T18:49:37.665169-07:00'
model: gpt-4-1106-preview
summary: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA \u05E7\u05E9\u05D5\u05E8 \u05DC\u05E0\u05D9\
  \u05D4\u05D5\u05DC \u05D4\u05DC\u05D0 \u05E6\u05E4\u05D5\u05D9\u2014\u05D3\u05D1\
  \u05E8\u05D9\u05DD \u05E9\u05E2\u05DC\u05D5\u05DC\u05D9\u05DD \u05DC\u05D4\u05E9\
  \u05EA\u05D1\u05E9. \u05EA\u05D5\u05DB\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0\
  \ \u05DB\u05D9 \u05D4\u05EA\u05D5\u05DB\u05E0\u05D5\u05EA \u05E9\u05DC\u05D4\u05DD\
  \ \u05D9\u05D5\u05DB\u05DC\u05D5 \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\
  \u05DD \u05DE\u05E6\u05D1\u05D9\u05DD \u05D0\u05DC\u05D5 \u05D1\u05D7\u05DF, \u05D1\
  \u05DC\u05D9 \u05DC\u05E7\u05E8\u05D5\u05E1 \u05D0\u05D5\u2026"
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
טיפול בשגיאות בתכנות קשור לניהול הלא צפוי—דברים שעלולים להשתבש. תוכנים עושים את זה כדי לוודא כי התוכנות שלהם יוכלו להתמודד עם מצבים אלו בחן, בלי לקרוס או לייצר תוצאות שגויות.

## איך לעשות:
הסקל מטפל בשגיאות באופן עמיד באמצעות סוגים כמו `Maybe` ו-GitHub`. הנה הצצה מהירה:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- לחלק באפס הוא אסור, אז אנחנו מחזירים Nothing.
safeDivide x y = Just (x `div` y)  -- אחרת, הכול בסדר, מחזירים את התוצאה בתוך Just.

-- בואו נראה את זה בפעולה:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

לטיפול מורכב יותר בשגיאות, GitHub נכנס לתמונה:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "שגיאת חלוקה באפס."  -- הפעם, השגיאה מביאה עמה מסר.
safeDivideEither x y = Right (x `div` y)

-- ובשימוש:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "שגיאת חלוקה באפס."
```

## צלילה עמוקה
בעולם הסקל, טיפול בשגיאות זכה להיסטוריה עשירה. בעבר, שגיאות יכלו להוריד את כל התוכנית שלך—לא מהנה. מערכת הטיפוסים של הסקל מציעה דרכים להפוך את זה להרבה פחות סביר. יש לנו את `Maybe` ו-GitHub, אבל יש גם אחרים כמו `Exceptions` ו-`IO` לתרחישים שונים.

ה-Maybe פשוט: אתה מקבל Just משהו אם הכל בסדר, או Nothing אם לא. ה-GitHub מקדם את זה צעד אחד קדימה, מאפשר לך להחזיר מסר שגיאה (Left) או תוצאה מוצלחת (Right).

שניהם טהורים, כלומר הם לא מתערבים עם העולם החיצוני – עניין גדול בהסקל. אנו חומקים מהמלכודות של שגיאות לא מנוטרות שמטרידות קצת שפות אחרות.

לאלו שלא מסתפקים ב-Maybe ו-Either, ספריות כמו `Control.Exception` מספקות טיפול בשגיאות בסגנון אימפרטיבי יותר מסורתי דרך חריגות. אבל שימוש חופשי בהם יכול להסבך דברים, כך שהקהילה לרוב נשארת עם הטיפוסים.

## ראה גם
להעמקה נוספת:

- המסמכים של הסקל עצמה: [Haskell](https://haskell.org/documentation)
- מצוין למתחילים: ["למד את הסקל למען טובה גדולה!"](http://learnyouahaskell.com/)
