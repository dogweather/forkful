---
title:                "הגדלת אותיות במחרוזת"
aliases: - /he/javascript/capitalizing-a-string.md
date:                  2024-02-03T19:06:16.822546-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
הופכים מחרוזת לאותיות ראשיות על ידי המרת התו הראשון של המחרוזת לאות רישית תוך שמירה על שאר התווים כפי שהם. ביצוע פעולה זו ב-JavaScript מקובל ביותר לצורך עיצוב קלטי משתמש, הצגת שמות או כותרות, והבטחת אחידות בטקסטים של ממשק המשתמש.

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
