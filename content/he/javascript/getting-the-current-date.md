---
title:                "Javascript: לקבלת התאריך הנוכחי"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה
הנה כמה סיבות מדוע ייתכן שתרצו לדעת את התאריך הנוכחי בפיתוח קוד ג'אבהסקריפט: לשמור תאריך מדויק עבור אירוע או טרחנות, לתיעוד הפעלות רלוונטיות בפקודת זמן לתוצאת הקוד, או פשוט למטרות משתמש בהתאם לאפליקציה.

## איך לעשות זאת
לקבלת תאריך נוכחי בג'אבהסקריפט, ישנן שתי אפשרויות. הראשונה היא להשתמש במתודת הנתונים המובנית, Date(), כך:

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

פלט:
```
"יום דצמבר 10 2019 21:55:23 GMT+0200 (שעון ישראל (קו ראשון))"
```

אם אתם רוצים לקבל תאריך מפורט יותר, תוכלו להשתמש במתודות נוספות כמו getFullYear(), getMonth(), getDay() ועוד, כך:

```Javascript
let currentYear = currentDate.getFullYear();
let currentMonth = currentDate.getMonth() + 1;
let currentDay = currentDate.getDay();
console.log(currentDay + "/" + currentMonth + "/" + currentYear);
```

פלט: 
```
"10/12/2019"
```

## ירידה עמוקה
לקבלת תאריך נוכחי בג'אבהסקריפט, החומר שלכם הוא משתנה מסוג Date. כך גם ניתן להחיל פעולות כמו חישובים או השוואות על התאריך הזה על ידי שימוש במתודות מובנות נוספות. עמוק יותר, תאריך נוכחי הוא אכן מספר של ימים או שניות מאזכורות בתחילת הזמן, הנקראת גם Unix Timestamp. אם אתם רוצים להדפיס את התאריך הנוכחי כ-Unix Timestamp, יש להשתמש במתודת getTime(), כך:

```Javascript
let currentTime = currentDate.getTime();
console.log(currentTime);
```

פלט:
```
1571822122394
```

ניתן להשתמש בערכים אלה כדי לייצר תנאים ולבצע פעולות יותר מתקדמות עם התאריך הנוכחי בקוד שלכם.

## ראו גם
למידע