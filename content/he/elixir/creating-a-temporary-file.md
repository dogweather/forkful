---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה & למה?
יצירת קובץ זמני היא דינמיקה שבה המתכנת יוצר קובץ שנמחק אחרי שהוא מסתיים לשימוש. מתכנתים עושים זאת במקרים שבהם הם צריכים לקחת נתונים, לעקוב אחריהם או לשנות אותם באופן זמני במהלך תוכנית.

## כיצד ל: 
בפלטפורמת Elixir, אתה יכול ליצור קובץ זמני באמצעות הפונקציה File.write. נסה קוד הבא:

```elixir
{:ok, path} = File.mktemp()
{:ok, file} = File.write(path, "temp data")
IO.puts("temporary file created with path: #{path}")
```
כניסת הקוד הזה תייצר קובץ זמני ותכתוב לתוך התוכן "temp data". הדרך לקבוע היא אופציונלית.

## צלילה עמוקה 
- היסטוריה: יצירה של קובצים זמניים הייתה חלק מתכנות המחשבים מאז התחלתו. הם מאפשרים שמירה על נתונים שאינם חיוניים לעבודה ארוכה טווח.
- אלטרנטיבות: בנוסף לפונקציה File.mktemp של Elixir, רבים משתמשים בספריות שלשיהם אפשרויות ליצירה של קבצים זמניים, כמו Floki, Filesystem וכו'.
- פרטי ביצוע: במחשבים רבים, קבצים זמניים מאוחסנים במיני-מערכת קבצים שנקראת tmpfs, ששימשה את הזיכרון האקראי לאחסון.

## ראה גם 
- [מתוך התיעוד של Elixir - File module](http://elixir-lang.org/docs/stable/elixir/File.html)
- [איך ליצור קבצים זמניים באמצעות Erlang - Stack Overflow](https://stackoverflow.com/questions/35520946/erlang-write-to-temporary-file)