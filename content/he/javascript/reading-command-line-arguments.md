---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא דרך שבה קוד ב-Javascript יכול לקבל בעת ריצה מידע מבחוץ. זה מאוד שימושי כאשר אנחנו רוצים להריץ את הקובץ איתו מידע שונה, כדי להראות תוצאות שונות.

## איך לעשות
הנה קטע קוד שמדגים את הקנייה בארגומנטים מקו הפקודה:
```Javascript
// index.js
const commandArgs = process.argv.slice(2);
console.log(commandArgs);
```
אם אתה מריץ את הקוד הזה עם `node index.js arg1 arg2 arg3`, התוצאה תהיה: `['arg1', 'arg2', 'arg3']`.

## צלילה עמוקה
(1) מהשיח ההיסטורי, ראשית קטעי הקוד המקובלים בארגומנטים משורת הפקודה הייתה מוגבלת לשפות הקוד הנמוך. עם הזמן, המאפיינים האלה התפשטו לשפות נוספות, כולל Javascript.
(2) למעשה, ישנם מודולים כמו commander and yargs מנותן את האופציה לקריאה של ארגומנטים מהקונסולה עם תמיכה בתסריטים מורכבים עם אפשרויות מגוונות.
(3) ביצועים של process.argv הם קצת מורכבים. בעצם, כשאנחנו מפעילים מאפליקציה בנוד, ה-API של V8 של הנוד מוסיף ארגומנטים למערך argv. 

## ראה גם
מעלים אחדים שמתייחסים לקריאה של ארגומנטים משורת הפקודה:
- מודעה של StackOverflow, שמתאר כיצד להשתמש ב-process.argv: https://stackoverflow.com/questions/4351521/how-do-i-pass-command-line-arguments-to-a-node-js-program
- מודול נאוד commander.js, שמקל על ניהול ארגומנטים: https://www.npmjs.com/package/commander