---
date: 2024-01-20 17:47:29.615740-07:00
description: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\u05DC\
  \u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E1\u05D5\u05E4\
  \u05E8\u05D9\u05DD \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\u05E9\
  \ \u05D1\u05D4. \u05E4\u05E8\u05D5\u05D2\u05E8\u05DE\u05E8\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DC\
  \u05D5\u05D8 \u05D1\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D0\u05DE\u05EA\
  \ \u05E7\u05DC\u05D8 \u05D5\u05DC\u05D1\u05E6\u05E2 \u05DE\u05E0\u05D9\u05E4\u05D5\
  \u05DC\u05E6\u05D9\u05D5\u05EA \u05E2\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA."
lastmod: '2024-03-11T00:14:12.180274-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\
  \u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E1\u05D5\u05E4\u05E8\
  \u05D9\u05DD \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\u05E9 \u05D1\
  \u05D4. \u05E4\u05E8\u05D5\u05D2\u05E8\u05DE\u05E8\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DC\u05D5\
  \u05D8 \u05D1\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D0\u05DE\u05EA \u05E7\
  \u05DC\u05D8 \u05D5\u05DC\u05D1\u05E6\u05E2 \u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\
  \u05E6\u05D9\u05D5\u05EA \u05E2\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  ."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
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
