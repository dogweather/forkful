---
title:    "Elixir: ממיר תאריך למחרוזת"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

איך מראה לנו את היתרונות של המרת תאריך למחרוזת באליקסיר. זה עשוי להיות שימושי במספר מקרים, כגון עבודה עם נתונים או יישומי שרת.

## איך לעשות זאת

כדי להמיר תאריך למחרוזת באליקסיר, יש להשתמש בפונקציית `to_string/1` עם פרמטר `~D` או `~d`. לדוגמה:

```elixir
date = ~D[2020-10-10]
string_date = to_string(date, ~D)
IO.inspect string_date
```

Output:
`"2020-10-10"`

כמו כן, ניתן גם להשתמש בפונקציות נוספות כמו `Date.to_iso8601/1` או `Calendar.ISO.date_to_gregorian_days/3` להמרת תאריך למחרוזת בפורמט מסוים.

## חפירה עמוקה

המרת תאריך למחרוזת באליקסיר די פשוטה, אבל כדאי לקחת בחשבון מספר מקרים אפשריים כמו טיפוס התאריך, הפורמט והאזור הגיאוגרפי. לדוגמה, במקרה של תאריך בפורמט אמריקאי (mm/dd/yy), ייתכן שתצטרכו להמירו לפורמט אירופאי (dd/mm/yy) לפני שתוכלו להמירו למחרוזת.

כדי לתמוך בפרמטים עמודים, ניתן להשתמש בפונקציית `Calendar.DateTime.format/3` עם פרמטר תאריך ופורמט מותאם אישית. כמו כן, ישנם מספר מודולים נוספים זמינים להמיר תאריך למחרוזת בפורמטים שונים.

## ראו גם

- [The Elixir Documentation on Dates and Times](https://hexdocs.pm/elixir/Calendar.html#functions)
- [A blog post on Formatting Dates in Elixir](https://www.poeticoding.com/formatting-dates-in-elixir/)