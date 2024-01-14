---
title:                "Javascript: השוואת שתי תאריכים"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מדוע

למה לעסוק בהשוואת שני תאריכים? לעיתים מתכנתים צריכים לבדוק עם אחד מהתאריכים הוא אחרי התאריך השני או לפניו בכדי לנהל בקטנה את הלוגיקה של התוכנית, לדוגמה, לבדוק מתי תאריך מסוים נפל במחזור, עדיין יותר טוב שנשתמש בטיפוס התאריך משום שהוא כולל כבר מגוון של תכונות הקשורות לטיפוס זה.

## כיצד לעשות זאת

הנה דוגמא לבידוד שני תאריכים בן שני ימים עם השתמשות ב-`moment` ו-`diff` מספרת (במקרה זה התאריך הראשון הוא תאריך נכון והתאריך השני הוא 2 ימי מטרה):

```Javascript
var date1 = moment();
var date2 = moment().add(2, 'd');
```

בלשוניים של כל תהליך ניתן לצפות במספר ימים שאחרי תאריך מעניין או לפניו:

```Javascript
var days = date2.diff(date1, 'days');
```

או בתור סטרינג מלא:

```Javascript
var text = date2.diff(date1, 'days') + ' days';
```

הפלט יהיה "2 ימים" או "מחר!"

## לעומק יותר

ישנן מספר דרכים לעשות טיפוס של תאריכים. למשל, כדי לפצצ יום מסוים, תוכלו להשתחרר מלהתמקד בכל תאריך בפרטיות שלו. חשוב מאוד לזכור כי השוואה בין מספר דרכים יכולה להיות רלוונטית לתכונות של טיפוס התאריך כגון הפעולות השונות המניים, הימים השבועיים, כל דבר לפני / לאחר חג.

## ראו גם

- [מסמך רשמי של Moment.js](https://momentjs.com/docs/) 
- [השוואת תאריכים עם JavaScript ](https://zellwk.com/blog/compare-dates-js/)
- [תחליף למודול השוואת תאריך בשפת JavaScript](https://www.npmjs.com/package/date-fns)