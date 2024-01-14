---
title:    "Javascript: לקבל את התאריך הנוכחי"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

מתחשק לכם ליצור אפליקציה שמציגה את התאריך והשעה הנוכחיים? אולי אתם רוצים ליצור משחק אינטראקטיבי שדורש מהמשתמש לנתח את הזמן הנוכחי? סימפוניות מסוימת? יישום זה הינו כלי חיוני אם תרצו ליצור תוכניות אינטראקטיביות.

## איך להשתמש

```
Javascript

// סעיף זה השתמש בפעולות מובנות לקבלת התאריך הנוכחי.
const getDate = new Date();
const currentDay = getDate.getDay();
const currentMonth = getDate.getMonth();
const currentYear = getDate.getFullYear();

// ניתן גם להציג תאריך נוסף כמו זמן נוכחי עם הפונקציה הבאה:
const getTime = new Date();
```

מקובל להציג את התאריך הנוכחי בפורמט מסוים כגון יום/חודש/שנה או שעות/דקות/שניות.

**תוצאה של משתנים התאריך הנוכחיים:**
```
שנה: 2021
חודש: 12
יום: 30
```

**תוצאה של הפונקציה הזמן הנוכחי:**
```
שעה: 15
דקות: 21
שניות: 45
```

## Deep Dive

למרבה המזל, ב-Javascript ישנן פעולות מובנות שמאפשרות לנו לקבל את התאריך הנוכחי בקלות ובפשטות. נוכל להשתמש בפעולות כגון `new Date()` ו- `getDay()`, `getMonth()`, ו- `getFullYear()`. נוכל גם להשתמש בפעולה `getHours()`, `getMinutes()`, ו- `getSeconds()` כדי לקבל את הזמן הנוכחי.

בנוסף, ניתן להשתמש בספריית חיצונית כגון Moment.js כדי לקבל יכולות נוספות כמו המרת זמני אזור זמן ופורמט מתאים.

## ראה גם

- [מדריך לפעולות התאריך והזמן ב-Javascript](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [תיעוד של הפונקציות המובנות של תאריך וזמן ב-Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
-