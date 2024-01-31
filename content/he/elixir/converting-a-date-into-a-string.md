---
title:                "המרת תאריך למחרוזת"
date:                  2024-01-20T17:37:02.230533-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
גורמים מורכבים כמו תאריכים צריכים ליהפוך למחרוזות כדי להיות נתונים קריאים. תכניתנים עושים את זה להצגה, תיעוד וחילופי נתונים.

## How to: (איך לעשות:)
```elixir
# יצירת תאריך
date = ~D[2023-04-14]

# המרה למחרוזת בפורמט ברירת מחדל
date_string_default = to_string(date)
IO.puts(date_string_default) # פלט: "2023-04-14"

# המרה למחרוזת באמצעות Calendar.strftime
date_string_custom = Date.to_string(date, "{YYYY}-{M}-{D}")
IO.puts(date_string_custom) # פלט: "2023-4-14"
```

## Deep Dive (עומק הנושא)
בעבר, יש מי שהשתמש במודולים חיצוניים כמו Timex כדי לעבוד עם תאריכים. כיום, Elixir מספק את מודול Date לעבודה מובנית עם תאריכים ולביצוע המרות. קיימים אף פורמטים רבים ומגוונים שניתן להשתמש בהם כדי להמיר תאריך למחרוזת.

Calendar.strftime מאפשר גמישות רבה בהמרת תאריכים למחרוזות עם פורמט מותאם אישית. Elixir משתמש ביכולות הזמן של Erlang, שעליהן הוא בנוי, ומרחיב אותן לשימוש ידידותי יותר.

## See Also (ראו גם)
- [Elixir Date Documentation](https://hexdocs.pm/elixir/Date.html) – הדוקומנטציה הרשמית עבור התמיכה בתאריכים ב-Elixir.
- [Erlang's Calendar Module](http://erlang.org/doc/man/calendar.html) – ידע קצת יותר עמוק על היכולות של Erlang, עליהן Elixir נשען.
