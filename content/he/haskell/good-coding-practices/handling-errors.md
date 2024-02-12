---
title:                "טיפול בשגיאות"
aliases:
- /he/haskell/handling-errors/
date:                  2024-01-26T00:56:23.122200-07:00
model:                 gpt-4-1106-preview
simple_title:         "טיפול בשגיאות"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/handling-errors.md"
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
