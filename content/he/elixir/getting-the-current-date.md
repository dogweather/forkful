---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליפת התאריך הנוכחי היא פעולה של קבלת התאריך והשעה של הרגע הנוכחי. מתכנתים משתמשים בזה לשם רישום, איתור טעויות וקביעת זמנים תקפים.

## איך:

אפשר להשיג את תאריך והשעה הנוכחיים באמצעות הפונקציה DateTime.utc_now().

```elixir
iex> DateTime.utc_now()
~U[2022-01-31T13:37:08.635876Z]
```
הפלט של הפקודה הוא תאריך ושעה בפורמט ISO8601.

## הצצה מעמיקה:

במקרה של DateTime.utc_now(), נכונה מהדורה 1.8 של Elixir.

לחלופות ל־DateTime.utc_now() כוללות את פונקציות התאריך והשעה של Erlang, כגון :erlang.localtime() ו:erlang.universaltime().
```elixir
iex> :erlang.localtime()
{{2022, 1, 31}, {13, 37, 8}}
iex> :erlang.universaltime()
{{2022, 1, 31}, {13, 37, 8}}
```
## ראה גם:

[דוקומנטציה של Elixir](https://hexdocs.pm/elixir/DateTime.html)
[דוקומנטציה של Erlang](https://erlang.org/doc/man/time_offset.html)