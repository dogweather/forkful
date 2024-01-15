---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Gleam: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

מדוע: קצר חרבור לעומת מידע רב יותר על כך למה מישהו יישגע לחשב תאריך בעתיד או בעבר.

איך לעשות: דוגמאות לקוד ותוצאות מפורמטות במתאם "Gleam ...". נכנס לדוגמאות הבאות נראה איך לחשב תאריך בעתיד ובעבר בעזרת הפונקציות המובנות במספריה של גלים.

כיצד זה עובד:
אז מה קורה כאשר אנחנו רוצים לחשב תאריך בעתיד או בעבר? כדי לעשות זאת, אנחנו משתמשים בפונקציות המובנות כמו `Date.add_days` ו-`Date.sub_days` כדי להוסיף או להחסיר ימים מתאריך קיים. ניתן גם להשתמש בפונקציות נוספות כמו `Date.add_weeks` ו-`Date.add_months` כדי להוסיף פרקי זמן נוספים לתאריך. הנה דוגמה שמציגה איך לחשב תאריך בעתיד בעזרת פונקציות אלה:

```Gleam
let tomorrow = Date.add_days(Date.now(), 1)
```

לחישוב תאריך בעבר, אנחנו משתמשים באותן הפונקציות כדי להוסיף או להחסיר ימים, שבועות או חודשים מתאריך קיים. הנה דוגמה שמציגה איך לחשב תאריך בעבר בעזרת פונקציות אלה:

```Gleam
let one_year_ago = Date.sub_years(Date.now(), 1)
let two_months_ago = Date.sub_months(Date.now(), 2)
```

כל אלה הדוגמאות יותר ניתן לשנות את התאריך המקורי בצורה פרגמטית על ידי שימוש בפונקציות נוספות כמו `Date.set_year`, `Date.set_month` ו-`Date.set_day`. כעת, בואו נבין טכניקות אלה בצורה עמוקה יותר.

ראו גם:
- [תיעוד של הפונקציות המובנות לחישוב תאריך בגלים](https://gleam.run/documentation/stdlib/date.html)
- [דגמאות נוספות לשימוש בפונקציות המובנ