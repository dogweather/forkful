---
title:    "Elixir: לקבל את התאריך הנוכחי"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

(Hebrew Translation)

# למה:
כדי לדעת את התאריך הנוכחי, ניתן להשתמש בקוד אלייקסיר ולהשתמש בפונקציית Date.utc_today (). זה יכול להיות שימושי כאשר אתה צריך להציג את התאריך הנוכחי באתר או אפליקציה.

## איך לעשות זאת:
```elixir
iex> Date.utc_today()
{:ok, ~U[2021-09-28 00:00:00Z]}
```

הפונקציה תחזיר ערך מסוג tuple המכיל את התאריך הנוכחי בפורמט UTC. אם אתה רוצה לקבל את התאריך בפורמט אחר, כמו איזור זמן מסוים, ניתן להשתמש בפונקציות נוספות כמו Date.utc_now / 1 או Date.now / 1.

## מעומק:
כאשר אלייקסיר מכין את התאריך הנוכחי באמצעות Date.utc_today (), הוא משתמש בפונקציית בסיס של Erlang, המכונה calendar.universal_time (). פונקציה זו מספקת את התאריך והזמן הנוכחי לפי האיזור הזמן המקומי של המחשב. כלומר, אם אתה משתמש בקוד זה במחשב שלך בישראל, התאריך שיוחזר יהיה בפורמט UTC +03:00 (איזור זמן של ישראל).

# ראה גם:
- [Elixir Calendar Module](https://hexdocs.pm/elixir/Calendar.html)
- [Erlang Calendar Module](http://erlang.org/doc/man/calendar.html)
- [Date and Time in Elixir](https://elixirschool.com/en/lessons/basics/date-time/)