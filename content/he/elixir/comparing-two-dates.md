---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה זה & למה?
השוואת שני תאריכים היא פעולה שבה אנו משווים בין הזמן שהתרחשו בו שני אירועים. מתכנתים עושים זאת כדי להבין את ההיסטוריה של אירועים ולהוסיף הקשר זמני.

## איך ל: 
הנה קטע קוד בסיסי שישווה בין שני תאריכים באמצעות השפה Elixir:

```elixir
date1 = Date.from_iso8601!("2022-01-01")
date2 = Date.from_iso8601!("2022-12-31")

case Date.compare(date1, date2) do
  :lt -> IO.puts "Date1 is earlier"
  :eq -> IO.puts "Both dates are the same"
  :gt -> IO.puts "Date1 is later"
end
```

## Deep Dive:
(1) היסטוריה: Elixir מבוססת על Erlang, שהייתה משמשת כראשונה בשנת 1986 לצרכי ניהול מערכות פז"א. מאז, נוספו לה הרבה תכונות חדשות, כולל השוואת תאריכים.

(2) חלופות: טיב השוואה שלתוכנה מספקת משמעה להשוואת אובייקטים מסוג אחר - DateTime. 

```elixir
datetime1 = DateTime.from_iso8601!("2022-01-01T24:00:00Z")
datetime2 = DateTime.from_iso8601!("2022-12-31T24:00:00Z")

case DateTime.compare(datetime1, datetime2) do
  :lt -> IO.puts "DateTime1 is earlier"
  :eq -> IO.puts "Both Datetime are the same"
  :gt -> IO.puts "DateTime1 is later"
end
```

(3) פרטי ביצוע: Date.compare משווה תאריכים לאחר שהומרים אותם למרשם. DateTime.compare משווה תאריכים ושעות, כולל יום, שנה, שעה, דקות, שניות, ואף מילישניות.

## ראה גם:
- המדריך הרשמי לשפת Elixir: [Elixir Date](https://elixir-lang.org/getting-started/basic-types.html#dates-and-time)
- קוד דוגמא של Elixir ב-GitHub: [Elixir Samples](https://github.com/elixir-lang/elixir/tree/v1.2.3/lib/elixir/test/elixir/fixtures)