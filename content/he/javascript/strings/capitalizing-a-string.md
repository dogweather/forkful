---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:16.822546-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-JavaScript,\
  \ \u05D0\u05D9\u05DF \u05E9\u05D9\u05D8\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA\
  \ \u05DC\u05D4\u05E4\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05EA \u05D1\u05D0\u05D5\u05E4\u05DF \u05D9\
  \u05E9\u05D9\u05E8, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF \u05DC\u05DE\u05DE\u05E9\
  \ \u05D6\u05D0\u05EA \u05D1\u05E7\u05DC\u05D5\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\
  \u05D5\u05EA \u05E9\u05D9\u05D8\u05D5\u05EA \u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\
  \u05E1\u05D9\u05E1\u05D9\u05D5\u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  ."
lastmod: '2024-03-13T22:44:39.947363-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-JavaScript, \u05D0\u05D9\u05DF \u05E9\u05D9\u05D8\u05D4 \u05DE\u05D5\
  \u05D1\u05E0\u05D9\u05EA \u05DC\u05D4\u05E4\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05EA \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05D9\u05E9\u05D9\u05E8, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF\
  \ \u05DC\u05DE\u05DE\u05E9 \u05D6\u05D0\u05EA \u05D1\u05E7\u05DC\u05D5\u05EA \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\u05D9\u05D8\u05D5\u05EA \u05D8\u05D9\
  \u05E4\u05D5\u05DC \u05D1\u05E1\u05D9\u05E1\u05D9\u05D5\u05EA \u05D1\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA."
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

## איך לעשות:
ב-JavaScript, אין שיטה מובנית להפוך מחרוזת לאות ראשית באופן ישיר, אך ניתן לממש זאת בקלות באמצעות שיטות טיפול בסיסיות במחרוזות.

### באמצעות JavaScript סטנדרטי
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // פלט: "Hello world"
```

### גרסת ES6
באמצעות תבניות מילוליות של ES6, ניתן לכתוב את הפונקציה בצורה קצרה יותר:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // פלט: "Hello ES6"
```

### באמצעות Lodash
Lodash היא ספריית עזר שלישית פופולרית המציעה מגוון רחב של פונקציות לטיפול ועבודה עם ערכי JavaScript, כולל מחרוזות. להפיכת מחרוזת לאות ראשית באמצעות Lodash:
```javascript
// קודם כל, התקן את lodash אם טרם עשית זאת: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // פלט: "Lodash example"
```
_שימו לב כיצד Lodash לא רק משנה את האות הראשונה לרישית, אלא גם משנה את שאר המחרוזת לאותיות קטנות, שונה מעט מהיישום הרגיל ב-JavaScript._

### באמצעות CSS (למטרות הצגה בלבד)
אם המטרה היא לעשות שימוש באותיות ראשיות לצורך הצגה בממשק המשתמש, ניתן להשתמש ב-CSS:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- מוצג כ-"Hello css" -->
```
**הערה:** שיטה זו משנה את האופן שבו הטקסט מופיע על הדף ללא שינוי של המחרוזת עצמה ב-JavaScript.
