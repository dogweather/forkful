---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
פירסום תאריך ממחרוזת הוא ההמרה של תאריך ממרכז טקסט למבנה נתונים שנותן ביכולת שלנו להפעיל עליו פונקציות מבנה תאריך. מתכנתים עושים את זה כדי לשלוט במידע בצורה יעילה יותר.

## איך ל:
```Haskell
import Data.Time
import System.Locale

readTime defaultTimeLocale "%Y-%m-%d" "2021-07-21" :: UTCTime
```
הפלט אמור להיראות כך:
```
2021-07-21 00:00:00 UTC
```
## צלילה עמוקה
הפירסום של תאריך ממחרוזת אף פעם לא היה פשוט במיוחד, עם המון פורמטים שונים וארוכים שנבנים בהנחות שונות. Haskell מתמקדת בבניית תאריך ממחרוזת באמצעות קביעת פורמט במערך, מה שמאפשר לנו לארגן נתונים שונים בנוסף לתאריכים.

חלופה לספרייה `Data.Time` היא `Data.Time.Format`, שמאפשרת גמישות יותר ותמיכה בכלים כמו `Parsec`.

הפרטים של המימוש של readTime εינם פשוטים, עם רזולוציית מיקרו (10-6 שניות), מה שמבטיח שיש לנו כל המידע שאנו צריכים, אך זה יכול להיות יותר מדי מה שאנחנו מצפים על תאריך.

## ראה גם
- [Official Haskell Library Documentation for Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)