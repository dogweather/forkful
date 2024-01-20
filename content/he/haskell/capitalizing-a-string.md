---
title:                "הגדלת אותיות במחרוזת"
html_title:           "Haskell: הגדלת אותיות במחרוזת"
simple_title:         "הגדלת אותיות במחרוזת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## המה ולמה?
הכתובת הגדולה במחרוזת אומרת להמיר את האות הראשונה של מילה לאות גדולה. תכנתים משתמשים בזה כדי להגיע לקוראים או לפניית שמות, כמו שמות ערים או אנשים.

## איך לעשות:
כדי להמיר את המחרוזת לתוך כתובה גדולה ב-Haskell, אנו משתמשים בפונקציה 'toUpper'. דוגמה:

```Haskell
import Data.Char(toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
```

יציאה מדגם:
```Haskell
capitalize "hello world"
-- "Hello world"
```

## צלילה עמוקה:
הפונקציה 'toUpper' מבית 'Data.Char' מגיעה מגרסה מוקדמת של השפה Haskell. ישנם שיטות חלופות להכתיב את התו הראשונ של מחרוזת, כמו שימוש במודולים של חיצים או applicative. לחלקם יתרונות וחסרונות, אך הפתרון שמוצג כאן הוא הגירסה הפשוטה והנקייה ביותר.

## עיין גם:
מספר מקורות שימושיים אחרים שעשויים לעזור לך לפתח את מיומנויות ה-Haskell שלך מציגים את כמות המעטה של הידע שלך, כולל:

- [[Haskell: סיכום מחרוזות]](http://learnyouahaskell.com/chapters/strings)
- [[LYAH: קישור גדול]](http://learnyouahaskell.com/chapters/functors-applicative-functors-and-monoids)
- [[Haskell Wiki: מונדות]](https://wiki.haskell.org/Monads_as_containers)