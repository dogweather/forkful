---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:39:10.943864-07:00
simple_title:         "ניתוח תאריך ממחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיבוד תאריך ממחרוזת הוא פעולה שבה מתרגמים מחרוזת המכילה תאריך לערך תאריך ממשי. תכניתנים עושים זאת כדי לאפשר יצירת, אחסון, והשוואות של תאריכים בצורה יעילה.

## איך לעשות:
```TypeScript
// ייבוא ספריית ליד
import * as moment from 'moment';

// דוגמא לעיבוד תאריך ממחרוזת באמצעות ספריית moment.js
const dateString: string = '2021-03-19';
const parsedDate: moment.Moment = moment(dateString);

console.log(parsedDate.format());  // פלט: 2021-03-19T00:00:00+02:00
```

```TypeScript
// עיבוד תאריך באמצעות האובייקט Date הפנימי של ג'אווה סקריפט
const dateString: string = '2021-03-19';
const parsedDate: Date = new Date(dateString);

console.log(parsedDate.toISOString()); // פלט: 2021-03-18T22:00:00.000Z
```

## עיון יסודי
לעיבוד תאריכים ממחרוזות שימושים היסטוריים רבים. האובייקט `Date` הוא הגישה הישירה בג'אווה סקריפט, אך עקב הבדלי אזורי זמן ופורמטים, כמעט תמיד מוטב להשתמש בספריות כגון `moment.js` או `date-fns`. בעוד `moment.js` שכיח מאוד, הוא גם כבד יותר מאשר חלק מהספריות האלטרנטיביות. בדור האחרון של פיתוח ווב, נטייה לעדיפות של ספריות קלות כמו `day.js` או יישום האינטרנשיונלזציה של ג'אווה סקריפט. מומלץ לבחור את הספריה בהתאם לדרישות האפליקציה ונפח התעבורה שמתוכנן.

## ראה גם
- [מדריך ל-API של תאריכים ב-MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [תיעוד של moment.js](https://momentjs.com/docs/)
- [הספריה date-fns](https://date-fns.org/)
- [הספריה day.js כאלטרנטיבה קלה ל-moment.js](https://day.js.org/)
