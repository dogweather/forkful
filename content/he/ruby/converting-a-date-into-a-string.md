---
title:                "המרת תאריך למחרוזת"
html_title:           "Ruby: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

אם אתם משתמשים בשפת תכנות רובי, סיכוי גדול שתצטרכו להמיר תאריך מסוים למחרוזת. פעולה זו חשובה לצורך בניית אפליקציות או אתרים תלת-מימדיים שיתלות בשימוש בתאריכים שונים. במאמר זה נלמד איך להמיר תאריך למחרוזת באמצעות שפת רובי.

## Why
Converting a date into a string is necessary when working with applications or websites that heavily rely on dates. It allows for easier manipulation and formatting of the date, making it more user-friendly.

## How To
```ruby
require 'date'
# Date object from string
str_date = "2021-01-01"
date = Date.parse(str_date)

# Convert to string
date_string = date.strftime("%d/%m/%y")
puts date_string # Output: 01/01/21

# Converting current date
current_date = Date.today
current_date_string = current_date.strftime("%B %d, %Y")
puts current_date_string # Output: January 28, 2021
```
כדי להמיר תאריך למחרוזת בשפת רובי, נמצא את הנקודת ההתחלה הרצויה בקובץ על ידי הוספת פקודת `require 'date'` לקוד. נגדיר תאריך מחרוזת באמצעות המשתנה `str_date` ונשתמש בפקודה `Date.parse` כדי להמיר את המחרוזת לאובייקט תאריך. לאחר מכן, נשתמש בפקודת `strftime` כדי להוציא את התאריך בתבנית שנרצה. בדוגמה שנתתי, אני השתמשתי בתבנית "%d/%m/%y" שמכניסה את השנה בפורמט של שני ספרות ואת החודש והיום בפתיחת ברירת המחדל.

כדי להמיר את התאריך הנוכחי למחרוזת, נשתמש בפקודה `Date.today` כדי להגיע לתאריך הנוכחי, ואחר כך נשתמש בפקודת `strftime` עם תבנית רצויה. בדוגמה שנתתי, התבנית "%B %d, %Y" מציגה את החודש באותיות מלאות ואת השנה בפורמט של ארבע ספרות.

## Deep Dive
כאמור, שימוש בפקודת `strftime` מאפשר לנו להוציא את התאריך בתבנית שאנחנו רוצים. ישנן מגוון רחב של ת