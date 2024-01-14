---
title:                "Elixir: ניצחון למחרת"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מדוע

עושים קיפיטליזציה של מחרוזת ב-Elixir? זהו פעולה נפוצה בכתיבת קוד ועשויה להיות שימושית כאשר נרצה להציג את המחרוזת המתאימה באופן מובן יותר למשתמש.

## איך לעשות

דוגמאות קוד ופלט המתארות את התהליך קיפיטליזציית המחרוזת:

```elixir
string = "hello world"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

הפלט של הדוגמא הבאה הוא "Hello world".

```elixir
string = "elixir is awesome"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

הפלט של הדוגמא הבאה הוא "Elixir is awesome".

בנוסף, ניתן לבצע קיפיטליזציה רק על האות הראשונה של המחרוזת, באמצעות הפונקציה `String.capitalize/2` והפרמטר `character`. למשל:

```elixir
string = "hi there"
capitalized_string = String.capitalize(string, "h")
IO.puts capitalized_string
```

הפלט של הדוגמא הבאה הוא "Hi there".

## עיון מעמיק

כעת, כשאנו יודעים איך לבצע קיפיטליזציה על מחרוזת ב-Elixir, ניתן לעיין במתודות נוספות שמאפשרות עבודה על מחרוזות בצורה מתקדמת יותר. למשל, המתודה `String.upcase/1` משנה את כל האותיות במחרוזת לאותיות גדולות, ו- `String.downcase/1` משנה את כל האותיות במחרוזת לאותיות קטנות. ניתן גם לעבוד על מחרוזות באמצעות יישומים של מתודות מחרוזתיות כמו `String.replace/4`, `String.contains?/2`, ועוד.

## ראה גם

- [מדריך רשמי: פונקציות מחרוזות ב-Elixir](https://hexdocs.pm/elixir/master/String.html)
- [שיעורים מתוחזקים ללימוד Elixir](https://elixirschool.com/)
- [קהילת Elixir העברית בפייסבוק](https://www.facebook.com/groups/200373093804986)