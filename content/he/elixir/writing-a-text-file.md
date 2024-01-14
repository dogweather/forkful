---
title:                "Elixir: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה
ניתן לכתוב קובץ טקסט ב-Elixir כחלק מתהליך התכנות עבור מטרות שונות, כגון כתיבת קוד פתרונות או יצירת תיעוד של קוד קיים.

## כיצד לעשות זאת
הנה דוגמאות של כתיבת קובץ טקסט ב-Elixir עם פלט דוגמה:

```elixir
# יצירת קובץ טקסט חדש וכתיבת "שלום עולם" בו
file = File.open("hello.txt", [:write])
IO.write(file, "שלום עולם")
File.close(file)

# קריאת קובץ טקסט קיים והדפסת התוכן שלו
file = File.open("hello.txt", [:read])
content = IO.read(file)
IO.puts(content)
File.close(file)
```

פלט:
```
שלום עולם
```

## לחקור עומק 
כתיבת קובץ טקסט ב-Elixir משתמשת בפונקציות ייעודיות המאפשרות פתיחה, קריאה וכתיבה של קבצים טקסט. ניתן להשתמש באפשרויות נוספות כמו פונקציות לניהול שגיאות או בפונקציות לניהול הרשאות גישה לקבצים.

## ראה גם
- https://elixirschool.com/en/lessons/basics/file-operations/
- https://hexdocs.pm/elixir/File.html