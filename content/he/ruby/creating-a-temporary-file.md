---
title:                "יצירת קובץ זמני"
html_title:           "Ruby: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני היא כלי חשוב בתכנות בשפת רובי. תוכלו להשתמש בו כדי ליצור קבצים שנמחקים בסיום התוכנית או כדי לעבוד עם נתונים מזמניים שלא נדרשים תמיד.

## איך לעשות זאת

הנה דוגמה ברובי עבור יצירת קובץ זמני וכתיבת הטקסט "Hello World" לתוכו:

```
Ruby File.open('tempfile.txt', 'w') do |file|
  file.write("Hello World")
end
```

ניתן לראות בפלט הבא שהטקסט נכתב בהצלחה לקובץ הזמני:

```
Hello World
```

ליצירת קובץ זמני ריק נוסף, ניתן להשתמש בפקודה `tempfile` המבוססת על הספרייה `tempfile`:

```
require 'tempfile'

file = Tempfile.new('tempfile', './')
puts file.path
```

בפלט ניתן לראות את נתיב הקובץ הזמני החדש שנוצר:

```
./tempfile20190910-5062-5gys1p
```

## עיון מעמיק

כאשר אתם יוצרים קובץ זמני, הוא נמחק כאשר התוכנית מסתיימת. תוכלו גם להשתמש בפקודת `unlink` כדי למחוק את הקובץ זמנית.

קובץ המשמש כקובץ זמני ניתן להצמד לתהליך באמצעות פקודת `unlink`, אך הוא מתווסף לפרויקט בהשתתפות ראויה באמצעות המתודה `cleanup`. תוכלו להשתמש בפקודה `tempfile.unlink` על קובץ זמני כדי להסיר אותו בסיום התוכנית על מנת לוודא שהוא מוחק את עצמו.

## ראה גם

- [ספריית `tempfile` ברובי](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [למדו רובי בעברית](https://rubylearning.com/satishtalim/ruby_method_invocation.html)