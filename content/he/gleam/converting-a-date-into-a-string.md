---
title:                "Gleam: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

למה אנשים מתעסקים במרחב תאריך וממירים אותו למחרוזת בשפת גלים?

## איך לעשות זאת

תכנונית הקוד הבאה מציגה דוגמא למרחב תאריך וכיצד להמירו למחרוזת בשפת גלים:

```Gleam
import gleam/canonical-date

let date = Date.now()
let string_date = canonical_date.to_date_time_string(date)

log(string_date)
```

פלט צפוי:

```
"2021-11-25T12:37:51.121Z"
```

## טיפול עמוק

המרחב המובנה `gleam/canonical-date` מכיל פונקציות נוספות לטיפול בתאריכים כגון המרה לתאריך שלאחר זמן מסוים או לחישוב התאריך הנוכחי, וכן פורמטים נוספים למחרוזת תאריך.

## ראה גם

- [מדריך לשפת גלים](https://gleam.run/blog/guide)
- [תיעוד עבור המרחב המובנה `gleam/canonical-date`](https://gleam.run/lib/gleam-canonical-date/latest)
- [מחברת בפייתון עם המרחב המובנה `gleam/canonical-date`](https://gleam.run/notebooks/python/cookbook-canonical-date.html)