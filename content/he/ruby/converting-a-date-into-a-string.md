---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת בשפת Ruby היא פעולה שממירה שֵׁם תאריך לסדר תווים. מתכנתים עושים זאת לצורך פורמט המקובל לחלק מהמשתמשים, או כאשר צריך לשמור נתונים בתוך מסדי נתונים כאשר מדובר בסוג מחרוזת.

## כיצד:
בשפת Ruby, אפשר להמיר תאריך למחרוזת באמצעות שיטות רבות. האחת מהן היא `strftime`.
```Ruby
date = Time.now 
string_date = date.strftime("%d/%m/%Y")
puts string_date
```
התוצאה של הקוד הזה תהיה מחרוזת שמייצגת את התאריך הנוכחי בפורמט DD/MM/YYYY.

## עומק יותר
### היסטוריה
`strftime` הוא תכנונה מלאת היסטוריה שמקורה בשפות תכנות עתיקות כמו C.

### חלופות
ב-Ruby קיימות שיטות נוספות רבות לעבודה עם תאריכים, כולל `iso8601` שמייצר תאריך בתקן ISO 8601.

```Ruby
date = Time.now 
string_date = date.iso8601
puts string_date
```
### פרטי מימוש
`strftime` שילוב של תווים ותווים מיוחדים שמייצגים את החלקים השונים של התאריך. קיים גם מנגנון יוצא דופן לתמיכה בפורמטים מקומיים של תאריך ושעה.

## ראה גם
- [תיעוד Ruby עבור `strftime`](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime)
- [תיעוד Ruby עבור `iso8601`](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html#method-i-iso8601)