---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"

category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט ב-Elixir מאפשרת שמירה והחלפה של מידע. תוכניתנים עושים את זה לשם יצירת לוגים, תצורה, או שמירת נתונים שאינם דורשים מסד נתונים.

## איך לעשות:
```elixir
# פתיחת (או יצירת) קובץ עם הרשאת כתיבה
{:ok, file} = File.open("example.txt", [:write])

# כתיבת טקסט לקובץ
IO.write(file, "שלום, עולם! הנה קובץ טקסט שנכתב באליקסיר.\n")

# סגירת הקובץ
File.close(file)
```
פלט:
קובץ בשם "example.txt" ייווצר עם המשפט "שלום, עולם! הנה קובץ טקסט שנכתב באליקסיר."

## ניתוח עמוק:
בעבר, שפות כמו Erlang (שElixir מבוסס עליה) היה בשימוש לעבודה עם קבצים. Elixir מספקת חוויית משתמש קלה יותר לכתיבה וקריאת קבצים. קיימות אלטרנטיבות כמו כתיבת נתונים לבסיסי נתונים או שמירת מידע בענן, אבל לקובץ טקסט יש יתרונות במיוחד בפשטות ונגישות. המימוש פועל על System File I/O API של בסיס המערכת הפעלה.

## ראו גם:
- [Elixir File Documentation](https://hexdocs.pm/elixir/File.html) – מסמכים רשמיים לעבודה עם קבצים בElixir.
- [IO Module in Elixir](https://hexdocs.pm/elixir/IO.html) – מידע על מודול IO לקלט/פלט בElixir.
- [Erlang File Handling](http://erlang.org/doc/man/file.html) – כיצד עובדים עם קבצים בErlang להשוואה.
