---
title:                "TypeScript: מרתיחת תאריך למחרוזת"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

תהייה: להסיבות הפשוטות מצורפות את הפלט וההפלט לתאריך שנמצא באמצעות קידוד ושירות תאריך. ממשק תאריך אחד מיאה תאריכים ב- TypeScript, כך שכדאי ללמוד את הנושא בפירוט.

## איך לעשות את זה

```TypeScript
// תאריך שנבחר
const myDate = new Date('2021/01/01');
// להמיר את התאריך למחרוזת עם תאריך מלא
const fullDate = `${myDate.getDate()}/${myDate.getMonth() + 1}/${myDate.getFullYear()}`;
console.log(fullDate); // פלט יהיה "01/01/2021"

// להמיר את התאריך למחרוזת עם תאריך מלא ושעה
const time = `${myDate.getHours()}:${myDate.getMinutes()}:${myDate.getSeconds()}`;
console.log(time); // פלט יהיה "00:00:00"
```

בדוגמה הנ"ל, אנו משתמשים במחרוזת דמיונית ("`/`") כדי להפריד בין החלקים של התאריך או השעה. קוד זה משתמש בפעולות של `getDate()`, `getFullYear()`, וכו ', כך שהוא יכול להשתמש במתודות לתאריך ושעה נכונים.

## חקירה עמוקה

עם TypeScript לשנות את התאריך המקורי המשתנה יכולה להיות מאוד מועילה כאשר אתה מנסה להציג מידע תאריך מפורט. במקרים שבהם תאריך מתחיל ב- 0, אנו יודעים שהתאריך לא נכתב בפורמט המלא. בעזרת קידוד TypeScript ותוכנית הפנימית שלו, אנו יכולים לגשת לתאריך ולשנות אותו לסדר חלקים במראה תאריך מלא.

בנוסף, ניתן להשתמש במתודות נוספות כמו `getDay()` לקבלת יום בשבוע מתאריך, `getMilliseconds()` לקבלת מילישניות, וכו ' כדי למלא תוסף מידע עם התאריך שנבחר.

## ראה גם

הנה כמה מאמרים נוספים שעשויים לעניין אותך:

- [תאריך מצפן כמו מחרוזת ב- TypeScript](https://link-to-external-article1)
-