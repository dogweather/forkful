---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:35:52.485236-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
פירסונג תאריך ממחרוזת הוא תהליך שבו מתרגמים נתונים מטקסטואליים למבנה נתונים מתאריך. תכנתים עושים זאת כדי לאפשר מניפולציה והשוואות עם נתוני תאריכים בתוך התוכנה.

## איך לעשות:
אייליקסיר מציעה כמה אופציות לפירסונג תאריכים. הנה דוגמא:

```elixir
defmodule DateParser do
  def parse_date(string) do
    {:ok, date} = NaiveDateTime.from_iso8601(string)
    date
  end
end

# דוגמא לשימוש
parsed_date = DateParser.parse_date("2025-03-14T11:52:39.123Z")
IO.inspect(parsed_date)
```

פלט דוגמא:

```elixir
~N[2025-03-14 11:52:39.123]
```

## צלילה עמוקה
פירסונג תאריכים היה חלק מתכנות מאז שנות ה-60. כל שפה מציגה ספריה סטנדרטית לעבודה עם תאריכים וזמנים. באייליקסיר, `NaiveDateTime` מספק המרה ממחרוזת בפורמט ISO8601 - סטנדרט בינלאומי לתאריכים וזמנים. חלופות כוללות ספריות כמו `Timex`, אשר מציעה יותר גמישות ופונקציות נוספות. בעבודה עם פורמטים אחרים שאינם ISO8601, יש לבחור בספריה המתאימה או ליצור פונקציית פירסונג מותאמת אישית.

## ראה גם
- [Elixir's NaiveDateTime documentation](https://hexdocs.pm/elixir/NaiveDateTime.html)
- [Timex - Elixir date/time library](https://hexdocs.pm/timex/Timex.html)
