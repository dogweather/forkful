---
title:                "Ruby: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

מחשבים בזמן הנוכחי הם בחלק מהיומיומיות שלנו כמעט בכל תחום. יכול להיות קשה לעקוב אחר כל המידע שאנו צריכים לנהל באופן אישי ומקצועי. נהוג להשתמש בתאריך הנוכחי כדי לעזור לנו לתכנן ולארגן את היום שלנו ולנהל את המידע הזה באופן אפקטיבי.

## כיצד

הקבלת תאריך נוכחי בפיתוח תוכנה ב-Ruby יכול להיות פשוטה ומהירה עם שימוש בפונקציה Date.today. ניתן להשתמש בקונסטרקטור הפנימי של החלקיק Date כדי ליצור תאריך חדש ולהציג אותו בפורמט שנרצה. לדוגמה:

```Ruby
today = Date.today
puts today # output: 2020-07-22
```

ניתן גם להשתמש בפונקציות כמו strftime על ידי ציון פורמט מותאם אישית כדי לקבל תאריך נוכחי בפורמט שלנו. למשל:

```Ruby
today = Date.today
puts today.strftime("%d/%m/%Y") # output: 22/07/2020
```

## נעזור שלום

Date.today הוא רק ההתחלה של ההנרטיב. כדי להחזיק את התאריכים במסד נתונים ולבצע מניפולציות נוספות, ניתן להשתמש במספר מחלקות תאריך נוספות כמו DateTime ו Time.

Markdown "ראה גם"

- [תיעוד ריבי רשמי עבור תאריך](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [מדריך התחלה ריבי לתאריך וזמן](https://www.rubyguides.com/2015/07/ruby-date-and-time/)
- [ספר "תכנות למתחילים עם ריבי"](http://ruby4kids.com/programming-with-ruby/)