---
date: 2024-01-20 17:41:32.753117-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E7\u05D5\u05D3\
  \ Ruby \u05E2\u05D1\u05D5\u05E8 \u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\
  \u05E5 \u05D6\u05DE\u05E0\u05D9."
lastmod: '2024-03-13T22:44:40.236985-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05D5\u05D3 Ruby \u05E2\u05D1\u05D5\u05E8 \u05D9\u05E6\u05D9\u05E8\
  \u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\u05D9."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

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
