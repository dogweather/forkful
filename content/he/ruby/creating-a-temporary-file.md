---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני משמשת לשמירה בזמני של נתונים שאינם בהכרח נחוצים לאורך זמן. מתכנתים משתמשים בקבצים זמניים בדרך כלל כאשר הם רוצים למנוע שימוש בזיכרון הראשי.

## איך ל:
ניתן ליצור קובץ זמני באמצעות המודול `Tempfile` ברובי. 
```Ruby
require 'tempfile'
temp_file = Tempfile.new('prefix', Dir.tmpdir)
temp_file.write("hello world")
temp_file.rewind
puts temp_file.read # Output: hello world
temp_file.close
temp_file.unlink
```
אנו מייצרים קובץ זמני, מכתיבים לו את הביטוי "hello world", סופרים את הפלט (בעזרת `puts`), סוגרים את הקובץ ומוחקים אותו.

## צלילה עמוקה
ייצירת קבצים זמניים מסבך בצורה דינאמית נתמכת מאז Ruby 1.8. 
- חלופות: במקום לשלוט קובץ זמני, אנו יכולים 일גם להשתמש בזיכרון ישירות, אך זה מסתורן לבעיות זיכרון אם הנתונים גדולים מדי.
- אם גודל הנתונים הוא לא חשאי, אנו יכולים להשתמש במחוללים של Ruby או במשוואות map/reduce במקום.
- ביצועים: אפשרות נוספת היא לכתוב נתונים תוך כדי יצירה של קובץ זמני. מכיוון שהנתונים אינם נשמרים בזיכרון המרכזי, אפשר לשמור כמות בלתי מוגבלת של נתונים.

## ראה גם:
- תיעוד רשמי ל- [`Tempfile`](https://ruby-doc.org/stdlib-2.5.1/libdoc/tempfile/rdoc/Tempfile.html)
- ["הדרך הנכונה לטפל בקבצים זמניים ב- Ruby"](https://www.jstorimer.com/blogs/workingwithcode/7766151-the-right-way-to-deal-with-temporary-files-in-ruby)
- ["יצירת קבצים זמניים ב- Ruby"](https://www.thoughtco.com/creating-temporary-files-in-ruby-2907739)