---
title:                "קבלת תאריך נוכחי"
html_title:           "Ruby: קבלת תאריך נוכחי"
simple_title:         "קבלת תאריך נוכחי"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

למה איש לנדלות לקבל את התאריך הנוכחי. יתכן שהם צריכים להתאים לאיזו מבנה תאריך, לעדכן את התאריך באפליקציה או פשוט מתעניינים לדעת את התאריך הנוכחי.

## איך לעשות זאת

```Ruby
# כדי לקבל את התאריך הנוכחי בפורמט תקין
Date.today

# כדי לקבל את התאריך הנוכחי עם היום בשבוע
Date.today.strftime("%A, %B %d, %Y")

# כדי לקבל תאריך עם תאריך ההולדת הקודמת שלך
birth_date = Date.new(1990, 12, 10)
birth_date.next_year(1) # Output: 1991-12-10
```

## חפירה עמוקה

הישתמשות בפונקציה `Date.today` נתחיל בלמעלה של המחלקה `Date`. פונקציה זו מחזירה את התאריך הנוכחי באופן חזותי וקריא. בנוסף, ניתן להשתמש בפונקציות נוספות כגון `strftime` כדי לתאם את פורמט התאריך לצרכיינו.

## ראו גם

- תעודת המידע הרשמית על פונקציות התאריך ב- Ruby: https://ruby-doc.org/stdlib-2.6.3/libdoc/date/rdoc/Date.html
- הספריה הנפוצה לניהול תאריכים וזמנים ב- Ruby: https://github.com/rubyworks/forklift