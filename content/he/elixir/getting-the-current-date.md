---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:14:03.998868-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
לקבל את התאריך הנוכחי זה פשוט לשאול את המחשב "איזה יום אנחנו?" זה מועיל במיוחד כשאתה רוצה לתייג יומנים, לבדוק זמנים או לתזמן משימות.

## איך לעשות:
ב-Elixir, שימוש במודול `Date` יתן את התאריך הנוכחי כך:

```Elixir
today = Date.utc_today()
IO.inspect(today)
```

פלט לדוגמה:
```
~D[2023-04-12]
```

אם אתה רוצה שעה ודקות:

```Elixir
time = DateTime.utc_now()
IO.inspect(time)
```

פלט לדוגמה:
```
#DateTime<2023-04-12 15:30:45.123456Z>
```

## עומק הנושא
בעבר, פונקציות זמן ותאריך לא היו חלק מ-Elixir עצמו, אלא דרך ספריות צד שלישי. עכשיו, עם מודולים כמו `Date` ו-`DateTime`, זה חלק מהשפה.

דרך נוספת לקבל תאריך וזמן היא להשתמש בספריית Timex שמציעה יותר גמישות ואופציות.

על מנת להבין איך המערכת שומרת ומנהלת זמנים, חשוב לדעת על המושגים של UTC ו-Time Zones. Elixir משתמשת ב-UTC (`Coordinated Universal Time`) ברירת מחדל.

## ראה גם
- [Elixir Date Documentation](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime Documentation](https://hexdocs.pm/elixir/DateTime.html)
- [Timex Documentation](https://hexdocs.pm/timex/Timex.html)
- [Understanding Date, Time, and Time Zones in Elixir](https://elixirschool.com/en/lessons/basics/date-time/)
- [UTC and Time Zone in Elixir](https://hexdocs.pm/elixir/1.12/Time.html)
