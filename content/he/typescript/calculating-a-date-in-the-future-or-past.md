---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "TypeScript: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# חישוב תאריך מהעתיד או מהעבר ב-TypeScript

## מה זה ולמה?

חישוב תאריך בעתיד או בעבר הוא פעולה שבה אנחנו מתאריכים או מקדימים תאריך באמצעות הוספה או חיסור של ימים, חודשים, שנים וכד'. התכנתים עושים זאת לשם שליטה וניתוח של נתוני זמן.

## איך בדיוק?

אז כיצד אפשר להשיג את זה ב-TypeScript? בואו נראה במקום לדבר על זה. 

```TypeScript
let date = new Date();
let futureDate = new Date();

// add 3 days to the date
futureDate.setDate(date.getDate() + 3);

console.log(futureDate);
```

הנה דוגמה להחזרת תאריך מהעבר:

```TypeScript
let date = new Date();
let pastDate = new Date();

// subtract 5 days from the date
pastDate.setDate(date.getDate() - 5);

console.log(pastDate);
```

## טיפול מעמיק

אין מידע היסטורי מעורר תשוקה על חישוב תאריך מהעתיד או מהעבר, אך ישנן דרכים אחרות להשיג את המטרה. 

```JavaScript
// Using Moment.js
const moment = require('moment');

let futureDate = moment().add(7, 'days');
let pastDate = moment().subtract(7, 'days');
```

יש לקחת בחשבון כי במרבית השימושים, השימוש ב-Moment.js ייתן תוצאה זהה. אך, המערכת הפנימית של JavaScript היא יעילה יותר על פי האבחנה של Google Lighthouse.

## ראה גם

אם אתם רוצים ללמד יותר על נושא, הנה קישורים שימיים:
- [w3schools - JavaScript Date setFullYear()](https://www.w3schools.com/jsref/jsref_setfullyear.asp)
- [Mozilla - JavaScript date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)