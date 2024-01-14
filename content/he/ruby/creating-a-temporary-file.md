---
title:                "Ruby: יצירת קובץ זמני"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# למה

יצירת קובץ זמני היא כלי שימושי עבור מתכנתים המצליח ליצור קבצים זמניים בסרטון והיעילות תלויה ביותר.

## איך לעשות

```Ruby
תיאור = "קובץ זמני"
קובץ = Tempfile.new(תיאור)
הדפס קובץ.נתונים.נתונים
קובץ.קרע!
```
```
תקציב.txt
```

## חפירה עמוקה

יצירת קובץ זמני מאפשרת למתכנתים ליצור קבצים זמניים בסיסיים שיכולים להשתמש כדי לקבל נתונים, לפתוח תיקיות ולעשות שינויים זמניים לנתונים ללא פגיעה בקבצי המקור. קבצים זמניים נוצרים למטרה מסוימת ונמחקים באופן אוטומטי כאשר הפעולה שבאים נגמרת.

# ראה גם

- https://ruby-doc.org/stdlib-3.0.1/libdoc/tempfile/rdoc/Tempfile.html
- https://www.rubyguides.com/2015/05/working-with-files-ruby/
- https://ruby-doc.org/core-2.7.0/Tempfile.html