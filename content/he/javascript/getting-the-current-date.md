---
title:                "לקבלת התאריך הנוכחי"
html_title:           "Javascript: לקבלת התאריך הנוכחי"
simple_title:         "לקבלת התאריך הנוכחי"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

קבלת התאריך הנוכחי היא פונקציה חשובה בתכנות המאפשרת לנו לקבל תאריך מדויק באופן אוטמטי. התאריך הנוכחי משמש כבסיס למגוון רחב של יישומים מתאימים, כמו יצירת לוגים, תזמון משימות ועוד.

## איך לעשות זאת?

קבלת התאריך הנוכחי ב-Javascript נעשית באמצעות פונקציית התאריך (Date). הנה כמה דוגמאות:

``` Javascript
var currentDate = new Date(); //תאריך מלא כולל שעה ותאריך
var currentDay = currentDate.getDay(); //יום בשבוע בצורה מספרית (ראשון-0, שני-1 וכן הלאה)
var currentMonth = currentDate.getMonth(); //חודש בשנה בצורה מספרית (ינואר-0, פברואר-1 וכן הלאה)
var currentYear = currentDate.getFullYear(); //שנה מלאה
```

תוכלו להתאמן ולצפות בתוצאות של קוד כזה כדי להבין טוב יותר את הערכים שנייבאים.

## חקירה עמוקה

ישנן כמה אפשרויות אלטרנטיביות לקבלת התאריך הנוכחי, כמו שימוש בספריות צד שלישי או כתיבה של פונקציה ייעודית משלכם. בכל זאת, פונקציית התאריך ב-Javascript היא פשוטה ויעילה ולכן נהוג להשתמש בה במידת הצורך.

אם אתם מעוניינים לקרוא עוד על היסטוריית פונקציית התאריך ב-Javascript ועל השימושים הנפוצים שלה, תוכלו למצוא מידע נוסף בקישורים המופיעים בסיום המאמר.

## ראו גם

* [MDN Web Docs על פונקציית התאריך ב-Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
* [W3Schools על פונקציית התאריך ב-Javascript](https://www.w3schools.com/jsref/jsref_obj_date.asp)
* [ManualsLib על כתיבת פונקציה ייעודית לקבלת התאריך הנוכחי ב-Javascript](https://www.manualslib.com/manual/67990/Sanyo-Cft2085d.html?page=88#manual)