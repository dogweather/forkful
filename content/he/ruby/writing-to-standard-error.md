---
title:    "Ruby: כתיבה לתקליט התקנה"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה?

כתיבה לפלט של שגיעות (שגיאות רציפות בתכניות) הוא דבר חשוב בתכנות בשפת Ruby. הדבר הוא יתרון ברור בעורך קוד ובהרצת קוד למטרת ניקוי קוד.

## איך?

הנה דוגמאות של כתיבה לפלט של שגיאות בשפת Ruby בתוך בלוקי קוד "```Ruby ... ```":

```
begin
  1 / 0
rescue ZeroDivisionError => e
  $stderr.puts "שגיאה: #{e.message}"
end
```

תוצאות הריצה של הקוד שלמעלה יהיו:

```
שגיאה: divided by 0
```

ניתן גם לשלוח פלט לקובץ באמצעות הפונקציה `puts`:

```
begin
  1 / 0
rescue ZeroDivisionError => e
  file = File.open("errors.txt", "w")
  file.puts "שגיאה: #{e.message}"
  file.close
end
```

והתוכן של הקובץ errors.txt יהיה:

```
שגיאה: divided by 0
```

## מצילים?

כתיבה לפלט של שגיאות נמצאת במשמעות התיעוד נוספת בשפת Ruby. כדי לאתר ולתקן שגיאות בקוד, נשתמש בכתיבה לפלט של שגיאות כדי למזער את זמן החיפוש ותיקון השגיאות. ניתן גם להשתמש בכתיבה לפלט של שגיאות כדי למזער את זמן הפיתוח ובדיקות היחידה של הקוד.

## ראה גם

- [אתר ריסטקלס לשגיאות בשפת Ruby](https://www.ruby-lang.org/en/documentation/errors/)
- [תיעוד ריסטקלס לכתיבה לפלט של שגיאות](https://ruby-doc.org/core-2.7.1/IO.html#method-c-warn)