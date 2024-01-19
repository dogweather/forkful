---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# מה זה ולמה? 
איחוד מחרוזות הוא התהליך של שילוב שניים או יותר מחרוזות למחרוזת אחת. התכנתים משתמשים בזה כדי לשנות, ליצור או לחבר מחרודות.

# איך:

## דרך أ:
איחוד דרך אופרטר '+':
```Javascript
var str1 = "שלום";
var str2 = " עולם";
var result = str1 + str2;
console.log(result); //"שלום עולם"
```
## דרך ب:
איחוד דרך `concat()`:
```Javascript
var str1 = "שלום";
var str2 =  " עולם";
var result = str1.concat(str2);
console.log(result); //"שלום עולם"
```

## דרך ג:
איחוד באמצעות תבניות מחרוזות:
```Javascript
var str1 = "שלום";
var str2 =  " עולם";
var result = `${str1}${str2}`;
console.log(result); //"שלום עולם"
```

# צלילה מעמיקה:
איחוד מחרוזות קיים כבר מאז הקוד הראשון של המחשב. בימים שלפנינו, קיימים רבות אלטרנטיביות לאיחוד מחרוזות. האופציה הראשונה היא באמצעות האופרטור של '+', מתודת `concat()` או באמצעות תבניות מחרוזות. 

עם זאת, ישנה הבחנה ביניהם. כאשר אתה משתמש באופרטור של '+', מה שאתה בעצם עושה הוא ליצור מחרוזת חדשה, ומערך של סיפורים מקוננים. בניגוד לתוכניות מחרוזות, אשר מתמרכזות ישירות מסביב לבעיית המחרוזת המקוננת.

# קשור גם:
1. [문자열 합치기](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
2. [Template strings](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
3. [Addition operator](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition)