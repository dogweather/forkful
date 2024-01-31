---
title:                "בדיקה האם ספרייה קיימת"
date:                  2024-01-19
simple_title:         "בדיקה האם ספרייה קיימת"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספרייה קיימת היא פעולת בדיקה שמחפשת לראות האם ספרייה נתונה קיימת במערכת הקבצים של המחשב. תכנתים עושים את זה כדי להמנע משגיאות במהלך קריאה או כתיבה לספריות.

## איך לעשות:
ב-Elixir, אפשר לבדוק אם ספרייה קיימת באמצעות המודול `File`:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "Directory exists!"
else
  IO.puts "Directory does not exist."
end
```

פלט לדוגמה:
```
Directory exists!
```
או
```
Directory does not exist.
```

## טבילה עמוקה
חלק מההיסטוריה של ניהול קבצים בתכנות כוללת פעולות פשוטות כמו זיהוי קבצים וספריות. אלטרנטיבות ל-`File.dir?` כוללות שימוש ב-`File.exists?` אשר יכול להחזיר `true` גם עבור קבצים. פרטי היישום ב-Elixir מבוססים על פונקציות הקיימות ב-Erlang, שעליו Elixir בנוי.

## ראו גם
- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Erlang file module documentation](https://erlang.org/doc/man/file.html)
