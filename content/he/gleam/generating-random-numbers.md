---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה זה? ולמה?
מספרים אקראיים הם מספרים שנבחרים באופן שאינו צפוי, אין להם שום דפוס או סדר מסויים. תכנתים משתמשים בהם כאשר יש צורך בחיווי של אירועים מקריים, כמו אירועי משחק, מדגם של נתונים אופקטיבי, ועוד.

## איך עושים זאת:
מקוד התוכנה הבא מדגים כיצד להשתמש במודול rand ליצירת מספרים אקראיים בגלים:

```gleam
import gleam/otp/process.{self}
import gleam/otp/random

let stronger_randomness =
  random.new_with_os_bytes_generator(
    self(), // The seed source process
    random.default_os_bytes_amount // The amount of random seed bytes to collect
  )

random.int(1, 10, stronger_randomness)
// => Ok(7)
```

כאן, `random.int(1, 10, stronger_randomness)` מייצר מספר אקראי בין 1 ל-10.

## צלילה עמוקה
מאז התחלת המחשבים, יצירת מספרים אקראיים הייתה משימה מרכזית. בזמן שהמתמטיקן John Von Neumann הציע את שיטת "ההמידה באמצע" כמתודה פשוטה ליצירת מספרים אקראיים, יותר ויותר שיטות מתקדמות ומדויקות פותחו עם השנים.

גלים משתמשת באלגוריתם של Mersenne Twister, אבל חשוב להבין שהמודול random בגלים מספק רק "אקראיות מבחינה הסטטיסטית" ולא לצורך יישומים של נתונים קריטיים מבחינת האבטחה, כמו מספרים סודיים במערכת ההצפנה.

## ראה גם
שפת התכנות Gleam מעודכנת לאורך הזמן. הצטרפו לאתר הרשמי כדי לישאר מעודכנים: https://gleam.run/
אתם יכולים גם לבדוק את התיעוד וחומרי הלמידה האחרים של Gleam.
אם אתם מעוניינים להעמיק יותר בנושא שליצירת מספרים אקראיים אתם מוזמנים לעיין באינטרנט ולמצוא מידע נוסף.