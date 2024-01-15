---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Javascript: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

תאר לעצמך שאתה צריך להפחית מסת הנסיעה שלך כדי לקבל חבילת טיול זולה לחו"ל. זה יהיה מאוד שימושי אם תוכל לחשב מתי תגיע לכיוון הטיול, כך שתוכל למקסם את המסת הנסיעה שלך. קלות להתקנה!

## איך לעשות זאת

```javascript
//הכנס את תאריך ההתחלה
let startDate = new Date('2021-10-15');

//הגדר כמה ימים מתלכדים (זו המסת הנסיעה המינימלית)
let travelDays = 7;

//חשב את תאריך הסיום באמצעות חישוב נוסף לפי מספר הימים המתלכדים
let endDate = new Date(startDate.getDate() + travelDays);

//הדפס את תאריך הסיום בפורמט מתאים
console.log(`תאריך הסיום של חבילת הטיול הוא: ${endDate.toDateString()}`);
```

פלט:

```
תאריך הסיום של חבילת הטיול הוא: Mon Oct 22 2021
```

## להעמיק

פונקציות כמו `getDate()` ו- `toDateString()` שימושיות לחישוב תאריכים בפורמטים שונים. ניתן להשתמש בתאריכי JavaScript בקלות נוספת כדי לייצר מועדים רבים, כולל תאריכים בעבר ובעתיד. לפרטים נוספים ניתן לעיין במדריכים הבאים:

- [תאריכי JavaScript נסיעה - W3Schools](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [תאריך JavaScript Calculator - FreeCodeCamp](https://www.freecodecamp.org/news/javascript-date-calculator/)
- [תיבת כלי של תאריך JavaScript - MDN](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/Date) 

## ראה גם

- [התחל עם JavaScript - מדריך למתחילים](https://www.udemy.com/course/javascript-beginners-complete-tutorial/)
- [מסמכי Markdown - GitHub](https://guides.github.com/features/mastering-markdown/)