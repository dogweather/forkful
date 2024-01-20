---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

המרת תאריך למחרוזת, היא תהליך שבו נותר המקור המספרי של תאריך מגורסא ומפוקמט, למחרוזת של תווים. תכנתים בוחרים לעשות זאת כדי למתג את התאריך לצפיה או לשמירה, כי זה משפר את התאמתיות המידע.

## איך לעשות: 

בהנחה שיש לך אובייקט Date, תוכל להשתמש במתודה `toLocaleDateString` כדי להמיר את תאריך למחרוזת. הנה איך זה עובד:

```TypeScript 
let date = new Date();
let dateString = date.toLocaleDateString();
console.log(dateString);
```

תוצאה צפויה:

``` 
"6/1/2022"
```

## צלילה:

המרת אובייקט התאריך למחרוזת היא משימה ותיקה בתכנות, ו־JavaScript מספקת כל מיני אמצעים כדי להקל המרה. על-אופן ממשקי הפרופיל, ישנם שלושה - `toString()`, `toDateString()`, ו`toLocaleDateString()` - שמדפיסים צורות שונות של מחרוזות התאריך.

디 Ιמנע מלהיות מתוחזק, `toLocaleDateString()` היא אפשרות מדהימה כי היא מאפשרת לך לשלוט את הנוסח של התאריך שיתאים לאזור גיאוגרפי מסוים. 

## ראה גם: 

*תיעוד MDN על פונקציות התאריך JavaScript והתחשיב שלהן:
https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/Date

*תיאור קצר לתכניתנים מתחילים בנושא תאריכים וזמנים ב־JavaScript:
https://www.digitalocean.com/community/tutorials/javascript-date-time-functions