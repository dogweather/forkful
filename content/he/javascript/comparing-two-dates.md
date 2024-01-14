---
title:                "Javascript: להשוואת שני תאריכים"
simple_title:         "להשוואת שני תאריכים"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

משהו מאתגר בתכנות הוא להשוות שתי תאריכים זמניים. ייתכן שתרצו לבדוק אם תאריך מסוים קורס כבר עבר, או לחשב את כמות הימים ביניהם. כתבה זו תעזור לכם ללמוד כיצד להשוות שתי תאריכים בקוד ג'אווהסקריפט.

## איך לעשות את זה

מצאנו כי ישנן שלוש שיטות להשוות שתי תאריכים בג'אווהסקריפט, ואנחנו נדגים כאן כמה דוגמאות כדי להראות את התהליך.

```Javascript
// אופציה א': השווה תאריך ישירות
let date1 = new Date('2021/08/01');
let date2 = new Date('2021/09/01');
if (date1.getTime() === date2.getTime()){
    console.log('התאריכים זהים!');
} else {
    console.log('התאריכים שונים.');
} // התוצאה תהיה: "התאריכים שונים."
```

```Javascript
// אופציה ב': השווה את השנה, החודש והיום של שני התאריכים
let date1 = new Date('2021/08/01');
let date2 = new Date('2021/09/01');
if (date1.getFullYear() === date2.getFullYear() && date1.getMonth() === date2.getMonth() && date1.getDate() === date2.getDate()){
    console.log('התאריכים זהים!');
} else {
    console.log('התאריכים שונים.');
} // התוצאה תהיה: "התאריכים שונים."
```

```Javascript
// אופציה ג': השווה את התאריך המלא כמשתנה
let date1 = new Date('2021/08/01');
let date2 = new Date('2021/09/01');
let fullDate1 = date1.getFullYear().toString() + date1.getMonth().toString() + date1.getDate().toString();
let fullDate2 = date2.getFullYear().toString() + date2.getMonth().toString() + date2.getDate().toString();
if (fullDate1 === fullDate2){
    console.log('התאריכים זהים!');
} else {
    console.log('התאריכים שונים.');
} // התוצאה תהיה: "התאריכים שונים."
```

## התרגול לעומק

בכתבה זו השתמשנו בכלים יסודיים כמו `getTime()`, `getFullYear()`, ו- `getMonth()` כדי להשוות שתי תאריכים. אך ישנן גם אפשרויות מתקדמות יותר להשוואת תאריכים, כגון שימוש בספריית Moment.js או כ