---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Elixir: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספרייה קיימת היא בדיקה שבה התכנית שלך מאמתת האם ספרייה מסוימת קיימת או לא. זה משחק תפקיד מרכזי במניעת שגיאות בעת ביצוע פעולות כמו לקרוא, לכתוב או לשנות קבצים בתוך ספרייה.

## איך לעשות:
נסתכל על קוד בשפת Elixir שבודק אם ספרייה קיימת.

```Elixir
File.dir?("path/to/directory")
```

הפונקציה `File.dir?` מחזירה `true` אם "path/to/directory" היא ספרייה, והיא מחזירה `false` אם היא לא ספרייה או אם היא לא קיימת.

## עומק יותר:
Elixir היא שפת תכנות שנוצרה ב-2011. היא משלבת את הגמישות של שפות תכנות פונקציונליות עם היכולות של Erlang לטפל במערכות בזמן אמת עם תמיכה של טולרנטיות לשגיאות.

אלטרנטיבות ל- `File.dir?` כוללות השתמש בפונקציה `File.stat!("path/to/directory").type` שמחזירה `:directory` אם הנתיב הוא ספרייה.

בתוך Elixir, הפונקציה `File.dir?` משתמשת ב- `:file.read_file_info/1` - פונקציה ב- Erlang שמחזירה מידע על פריט במערכת הקבצים.

## ראה גם:
- מדריך לשפת Elixir: https://elixir-lang.org/getting-started/introduction.html
- תיעוד Elixir: https://hexdocs.pm/elixir/File.html#dir?/1
- פונקציה ב-Erlang `:file.read_file_info/1`: https://erlang.org/doc/man/file.html#read_file_info-1