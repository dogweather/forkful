---
title:                "Ruby: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה
המרת תאריך למחרוזת היא כלי חיוני בתכנות בשפת רובי כי זה מאפשר למשתמש להציג תאריך בפורמט שנוח לו.

## איך לעשות זאת
Transforming a date into a string in Ruby is a simple process that can be done using the `strftime` method. This method takes in a format string and converts a date object into a string based on that format.

```Ruby
date = Time.new(2021, 9, 15) # יצירת אובייקט תאריך
date_string = date.strftime("%m/%d/%Y") # המרת התאריך למחרוזת בפורמט חודש/יום/שנה
puts date_string # תוצאה: 09/15/2021
```

הנה כמה טיפים נוספים לשימוש במתודה `strftime`:
- `%B` מציג את שם החודש מלא
- `%A` מציג את שם היום בשבוע מלא
- `%j` מציג את היום של השנה

למשל:
```Ruby
date = Time.new(2021, 9, 15)
date_string = date.strftime("%A, %B %j") # המרת התאריך למחרוזת בפורמט יום בשבוע, חודש ויום של השנה
puts date_string # תוצאה: Wednesday, September 258
```

## השקעה עמוקה
אם אתה מתעמק יותר בהמרת תאריך למחרוזת, תגלה שרובי משתמש בפונקציית C `strftime` כדי לבצע את תהליך ההמרה. בגלל זה, אם ברצונך להשתמש בפונקציות שאינן נתמכות רשמית על ידי רובי, כגון האפשרות להציג תאריך בפורמט של השעון ה-12, תצטרך להתאים את הפונקצייה המקורית וליצור אתה מחדש.

## ראה כמו כן
- [מדריך לפורמטי תאריך ושעה](https://strftimer.com/)
- [מאמר על כמה טיפים להשתמש בפונקציית `strftime` של רובי](https://www.rubyguides.com/2015/06/ruby-time/)
- [מסמך רשמי על פונקציות המוטבעות של רובי](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)

# ראה כמו כן