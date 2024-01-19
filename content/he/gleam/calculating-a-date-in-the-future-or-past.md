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

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא ביצוע פעולות חשבון על תאריכים. מתכנתים עשויים לקיים זאת למגוון סיבות, כמו לנהל את זרימת הזמן ביישומי תוכנה.

## איך?

בדוגמה הבאה, אנחנו מחשבים תאריך 30 ימים מהיום ב-Gleam:

```Gleam
import gleam/erlang.{date, add_days}

fn get_date_in_future(days: Int) -> tuple(Int, Int, Int) {
  date
  |> add_days(days)
}
```

אם נפעיל זאת עם הקלט של `30`, נקבל:

```Gleam
assert Ok((2022, 12, 23)) = get_date_in_future(30)
```

## צלילה עמוקה
חישוב תאריכים לעתיד או לעבר הוא מנגנון מסוף עננים שמאפשר למתכנתים לשלוט ב- "זמן" של היישום שלהם. הוא היה נמשך זמן רב, שופר עם מערכת הזמן של יוניקס בשנות ה-70, ועכשיו מאומץ ברחבי עולם התכנות.

אפשרויות חלופיות כוללות שימוש בקבועים (כמו "תאריך היום") או בפונקציות מובנות אחרות בשפה שלך.

באמת, Gleam מבצע זאת על ידי שימוש במתכנת התאריכים של Erlang, מה שנותן לו תמיכה מובנית בחישובים מורכבים עם תאריכים.

## ראה גם
[תיעוד Gleam לעבודה עם תאריכים](https://hexdocs.pm/gleam_erlang/gleam/erlang.html#date/0)
[מדריך לחישוב תאריכים ב-Erlang](https://erlang.org/doc/apps/erts/time_correction.html#date-and-time)