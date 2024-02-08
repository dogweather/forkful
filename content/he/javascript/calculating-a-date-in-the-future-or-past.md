---
title:                "חישוב תאריך בעתיד או בעבר"
aliases:
- he/javascript/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:48.356745-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר זהו לוקח תאריך ומוסיף או מחסיר ממנו זמן. מתכנתים עושים זאת לתזמון אירועים, התראות, וסינכרון נתונים.

## איך לעשות:
```Javascript
// חישוב תאריך עתידי - עוד 10 ימים מהיום
let today = new Date();
let futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);
console.log(futureDate);

// חישוב תאריך בעבר - 5 ימים לפני היום
let pastDate = new Date(today);
pastDate.setDate(today.getDate() - 5);
console.log(pastDate);
```
פלט לדוגמא:
```
2023-04-12T12:00:00.000Z (התאריך העתידי)
2023-03-28T12:00:00.000Z (התאריך בעבר)
```

## צלילה עמוקה:
חישוב תאריכים ב-JS היה תמיד מורכב עקב מורכבות שעון העולם. התמיכה באזורי זמן ושינויים אוטומטיים ל-UTC יכולה לקרות אוטומטית, אבל לפעמים זה עלול ליצור בעיות. אלטרנטיבות כוללות ספריות כמו Moment.js ו-date-fns, אשר מציעות חווית משתמש נוחה יותר ופונקציונאליות רחבה יותר. קוד לוקאלי צריך לתמוך בתאריכים עבריים וטיפול באירועים היסטוריים ודתיים.

## ראה גם:
- [MDN - Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns Documentation](https://date-fns.org/)
- [Moment.js Documentation](https://momentjs.com/docs/#/use-it/)
- [Hebrew calendar programming](https://www.hebcal.com/home/developer-apis)
