---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:47:29.615740-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת היא התהליך שבו אנחנו סופרים כמה תווים יש בה. פרוגרמרים עושים את זה כדי לשלוט בנתונים, לאמת קלט ולבצע מניפולציות על מחרוזות.

## איך לעשות:
ב-Elixir, הפונקציה `String.length/1` משמשת למציאת אורך של מחרוזת.

```elixir
my_string = "שלום עולם"
length = String.length(my_string)
IO.puts(length)
```

פלט:
```
9
```

## ניתוח עמוק:
בעבר, פונקציות כמו `length/1` היו שונות בשפות תכנות ספציפיות ולעיתים דרשו טיפול ידני במילון הקוד (encoding). ב-Elixir, פונקציית `String.length/1` מטפלת בצורה אוטומטית במחרוזות מקודדות ב-UTF-8, שהם סטנדרט ברוב המערכות המודרניות. חלופות? יש פונקציות כמו `byte_size/1` שמחזירה את גודל המחרוזת בבתים ולא בתווים. השימוש בה תלוי בדרישה הספציפית – אם צריך גודל פיזי ולא מספר תווים לוגיים. 

## ראה גם:
- [Elixir's String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Understanding UTF-8 and Character Encoding in Elixir](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/strings/)
