---
title:                "כתיבה לקובץ טקסט"
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לקובץ טקסט ב-Ruby היא פשוט ליצירת תיעוד, שמירת התוצאות, או לעבודה עם נתונים ביני ריצות של תוכנית. תכנית עושה את זה כדי לשמור נתונים שיהיו נגישים גם אחרי שהיא תסתיים.

## איך לעשות:
כדי לכתוב לקובץ טקסט, השתמש בקוד הבא:
```Ruby
File.open("example.txt", "w") do |file|
  file.puts "שלום, זה שורה חדשה בקובץ!"
end
```
תוצאת הדוגמה תיצור או תדרוס קובץ בשם `example.txt` עם הטקסט `"שלום, זה שורה חדשה בקובץ!"`.

## צלילה לעומק:
כתיבה לקבצים היא פעולה בסיסית שקיימת כבר מהימים הראשונים של המחשוב. בימינו, ישנם גם חלופות כגון עבודה עם מסדי נתונים כמו SQLite או שימוש במערכות קבצים מבוזרות כמו AWS S3. כאשר כותבים לקובץ, חשוב להבין את האפשרויות השונות של 'modes', כמו "w" לכתיבה (שמדרס קובץ קיים) או "a" להוספה (שמוסיף טקסט לקובץ קיים).

## ראו גם:
למידע נוסף על כתיבה לקבצים ב-Ruby, בקרו ב:
- [IO Class - Ruby-Doc.org](https://ruby-doc.org/core/IO.html)
- [File Class - Ruby-Doc.org](https://ruby-doc.org/core/File.html)
- [מדריך לכתיבת קבצים - RubyGuides](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
