---
title:                "יצירת קובץ זמני"
aliases:
- /he/ruby/creating-a-temporary-file/
date:                  2024-01-20T17:41:32.753117-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא תהליך שבו אנו יוצרים קובץ שנועד להימחק לאחר שימוש קצרצר. תכניתנים עושים זאת כאשר הם זקוקים לאחסון נתונים באופן זמני, למשל, לצורך עיבוד נתונים או לשיתוף פעולה עם תהליכים אחרים.

## איך לעשות:
קוד Ruby עבור יצירת קובץ זמני:
```Ruby
require 'tempfile'

temp_file = Tempfile.new('my_temp_file')
temp_file.write("שלום, עולם!")
temp_file.rewind # חוזרים להתחלת הקובץ כדי לקרוא ממנו
puts temp_file.read # קריאת התוכן שנכתב
temp_file.close # סגירת הקובץ
temp_file.unlink # מחיקת הקובץ מהדיסק
```
פלט דוגמא:
```
שלום, עולם!
```

## צלילה לעומק
יצירת קבצים זמניים היא פעולה נפוצה במערכות הפעלה כבר עשרות שנים. קבצים אלו מוחזקים בדרך כלל בתיקייה זמנית ונועדו לא להשאיר "זבל" אחרי סגירת התהליך שיצר אותם. ברובי, המחלקה `Tempfile` מנהלת זאת בשבילנו, אבל יכולים גם להשתמש בכלים כמו `File` ו-`Dir` לטיפול ידני. `Tempfile` מבוססת על מחלקה מקורית בשם `DelegateClass` של `IO` – כל זאת מבטיח שהקובץ ינוהל כראוי וימחק בסוף. יש לשים לב שבסביבות רבות-משתתפים או בכל מקרה של שימוש שגוי, ייתכן שקבצים "זמניים" לא יימחקו כפי שציפינו - לכן כדאי להשתמש בקפידה.

## ראה גם
קישורים למקורות הקשורים לנושא:
- [מדריך מהיר למחלקת Tempfile ב-Ruby](https://www.rubydoc.info/stdlib/tempfile/Tempfile)
- [מבט על מחלקת File ושיטותיה](https://ruby-doc.org/core-3.0.0/File.html)
- [מבט על מחלקת Dir ושיטותיה](https://ruby-doc.org/core-3.0.0/Dir.html)
