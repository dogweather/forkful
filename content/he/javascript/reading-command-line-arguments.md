---
title:                "קריאת ארגומנטים משורת הפקודה"
html_title:           "Javascript: קריאת ארגומנטים משורת הפקודה"
simple_title:         "קריאת ארגומנטים משורת הפקודה"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

קרא כתבה בפייתון 

## מה ולמה? 
קריאת פקודות קו מפקודה היא פעולה שמאפשרת למפתחים לקרוא פרמטרים שמיועדים לתוכנית שלהם מדרך השורת פקודה. הפעולה הזו עשויה להתבצע למגוון מטרות, כגון שינוי התנהגות של התוכנית בהתאם לפרמטרים שהועברו אליה, או לקבל את ערכי הפרמטרים ולעשות איתם משהו.

## איך לעשות זאת: 
כדי לקרוא פקודות מתוך השורת פקודה בפייתון, צריך להשתמש במודול בשם "sys". ניתן להכניס קוד לתוך המודול הזה, כך שהוא יכיל את הפרמטרים שהועברו אליו. הנה דוגמה של קוד המדגים כיצד לקרוא את הפרמטרים בעזרת המודול "sys":

```Javascript
var sys = require('sys');
sys.argv.forEach(function (arg, index) {
  console.log('Argument #' + index + ': ' + arg);
});
```

הפלט המתקבל כאשר מריצים את התוכנית עם הפרמטרים "Hello" ו-"World" הוא:

Argument #0: node
Argument #1: index.js
Argument #2: Hello
Argument #3: World

## טכניקות מתקדמות: 
בעבר, קריאת פקודות מתוך השורת פקודה הייתה נעשית על ידי השתמשות במשתנה מיוחד הנקרא "argv". אולם, בהמשך הפיתוח נמצאת הרבה טכניקות יותר מתקדמות לקריאת פקודות, כגון "commander" ו-"yargs". התוכניות הללו עוזרות לקוד שלכם להתמודד עם פרמטרים שונים עם שפחתיות ונוחות רבות יותר.

## ראו גם: 
למידע נוסף על קריאת פקודות מתוך השורת פקודה בפייתון, ניתן לבקר באתר הבא: https://nodejs.org/docs/latest-v10.x/api/process.html#process_process_argv

כמו כן, ניתן למצוא מידע נוסף על טכניקות מתקדמות לקריאת פקודות בפייתון באתר הבא: https://www.npmjs.com/package/commander 
https://www.npmjs.com/package/yargs