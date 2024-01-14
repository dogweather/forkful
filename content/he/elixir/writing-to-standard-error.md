---
title:                "Elixir: כתיבה לתקליטור המקוננטז (Standard Error)"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

כתיבת הודעות לפלט השגיאה הוא כלי חשוב בתכנות ב-Elixir והוא מאפשר לנו לזהות ולטפל בשגיאות באופן יעיל יותר. במאמר זה נלמד כיצד לכתוב לפלט השגיאה באמצעות Elixir.

## איך לעשות זאת

כדי לכתוב הודעות לפלט השגיאה, נשתמש בפונקציית IO.puts/2 כך:

```Elixir
IO.puts(:stderr, "הודעת שגיאה")
```

תוצאה:

```
הודעת שגיאה
```

ניתן להשתמש גם בפונקציית IO.inspect/2 כדי לנתח ערך ולהדפיס את תוצאת הניתוח לפלט השגיאה, כך:

```Elixir
IO.inspect(:error, label: "סוג השגיאה:", io: :stderr)
```

תוצאה:

```
סוג השגיאה: :error
```

ניתן להשתמש גם במהדורת פונקציית IO.puts/2 המקוצרת, כך:

```Elixir
:stderr |> IO.puts("הודעת שגיאה")
```

תוצאה:

```
הודעת שגיאה
```

## חקירה עמוקה

כתיבת הודעות לפלט השגיאה יכול לתרום לתהליך הדיבוג והמציאת באגים באופן יעיל יותר. בנוסף, זהו כלי שימושי בהמבחן ואיתור עיקולים. כדי להרחיב את הידע הלא רק על הפעולות הבסיסיות של כתיבת הודעות לפלט השגיאה, ניתן לחקור עוד על כתיבת דוחות השגיאות ב-Elixir ואיך לטפל בשגיאות על ידי יצירת בלוקי try-catch.

## ראה גם

- [כתיבת דוחות השגיאות ב-Elixir עם Exception](https://elixir-lang.org/getting-started/exceptions.html)
- [בלוקי try-catch ב-Elixir](https://medium.com/@elvinyung/try-catch-in-elixir-fab7242e6512)