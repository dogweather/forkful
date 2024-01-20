---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

---
## מה זה ולמה?

לקבלת תאריך נוכחי ב JavaScript היא תוכנה לאחזור ותצוגה של תאריך ושעה כרגע. מתכנתים משתמשים בזה במגוון של שימושים, כולל אודיט, תיעוד, תיעוד ועוד.

## איך לעשות:

קודים לדוגמה:

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

התוצאה המרשמת:

```Javascript
// "2022-03-14T21:12:31.001Z"
```

אם אתה רוצה רק את התאריך ללא הזמן:

```Javascript
let currentDate = new Date();
console.log(currentDate.toDateString());
```

תוצאה:

```Javascript
// "Tue Mar 14 2022"
```

## עומק ראייה:

אחיזה של התאריך הנוכחי הוא חלק מהיסטוריה של תיכנות הקוד פתוח. מאז הימים הראשונים של תיכנות, מתכנתים היו זקוקים למעקב היסטורי לעסקאות ואירועים אחרים. לחלופין, אפשר להשתמש בספריות צד שלישי כמו Moment.js או Date-fns לניהול אחיזת תאריך וזמן. אבל עם השיפורים ב JavaScript, אתה יכול כעת להשתמש ברוב המערכות עם המובנה new Date(). 

## ראה גם:

1. [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js](https://momentjs.com/)
3. [Date-fns - Modern JavaScript date utility library](https://date-fns.org/)