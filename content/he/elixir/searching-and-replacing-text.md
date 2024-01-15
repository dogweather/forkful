---
title:                "חיפוש והחלפת טקסטים"
html_title:           "Elixir: חיפוש והחלפת טקסטים"
simple_title:         "חיפוש והחלפת טקסטים"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה:

החיפוש וההחלפה של טקסט הוא כלי חשוב בתכנות שימושי במיוחד כאשר מתעסקים עם קבצי טקסט גדולים או כאשר חייבים לבצע מעברים מהירים של שפת מקור מסוימת לאחרת. באמצעות חיפוש והחלפה, ניתן לחפש מחרוזות מסוימות בתוך קובץ טקסט ולהחליף אותן במחרוזות אחרות תוך קלות ומהירות.

## איך לעשות:

הקוד המצורף מראה כיצד לבצע חיפוש והחלפה תוך שימוש בשפת Elixir. בקוד נתונה מחרוזת איתה נבצע את החיפוש וההחלפה, ואת התוצאה נדפיס בעזרת הפונקציה IO.puts:

```Elixir
text = "שלום עולם"
put "מחרוזת ראשונה: #{text}"

new_text = String.replace(text, "עולם", "כולם")
IO.puts "מחרוזת חדשה: #{new_text}"
```

תוצאה:

```
מחרוזת ראשונה: שלום עולם
מחרוזת חדשה: שלום כולם
```

## טיול עמוק:

באמצעות הפונקציה String.replace ניתן לבצע חיפוש והחלפה של מחרוזת בתוך מחרוזת אחרת. ניתן להעביר לפונקציה גם פרמטר נוסף שיכול לציין כמה פעמים חיפש למחרוזת ולהחליף אותה. כמו כן, באמצעות פונקציות אחרות כמו String.split ו- Enum.map ניתן לבצע חיפוש והחלפה בדיוק בהקשר הרצוי.

## ראה גם:

- [Documentation for String.replace in Elixir](https://hexdocs.pm/elixir/String.html#replace/4)
- [Article on using Elixir for text processing and manipulation](https://www.itnext.io/text-processing-in-elixir-4eaac3cd69b7)
- [Blog post on using Elixir for data processing](https://michal.muskala.eu/2017/02/19/efficient-data-processing-with-elixir.html)