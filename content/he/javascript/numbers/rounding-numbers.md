---
date: 2024-01-26 03:46:34.037404-07:00
description: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05D4\u05D5\u05D0 \u05E7\u05D9\u05E6\
  \u05D5\u05E5 \u05D4\u05E8\u05E2\u05E9 \u05DC\u05D0\u05D7\u05E8 \u05E0\u05E7\u05D5\
  \u05D3\u05D4 \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA \u05D1\u05DE\u05E1\u05E4\u05E8\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05D3\u05D9\u05D5\u05E7\
  , \u05DC\u05E0\u05D4\u05DC \u05D6\u05D9\u05DB\u05E8\u05D5\u05DF, \u05D0\u05D5 \u05DC\
  \u05D4\u05E4\u05D5\u05DA \u05D0\u05EA \u05D4\u05E4\u05DC\u05D8 \u05DC\u05E0\u05D5\
  \u05D7 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 - \u05DB\u05DE\u05D5 \u05DC\u05D4\u05E4\
  \u05D5\u05DA 2.998 \u05DC-3 \u05E0\u05E7\u05D9."
lastmod: '2024-03-13T22:44:39.966205-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05D4\u05D5\u05D0 \u05E7\u05D9\u05E6\u05D5\
  \u05E5 \u05D4\u05E8\u05E2\u05E9 \u05DC\u05D0\u05D7\u05E8 \u05E0\u05E7\u05D5\u05D3\
  \u05D4 \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA \u05D1\u05DE\u05E1\u05E4\u05E8. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD \u05DB\
  \u05D3\u05D9 \u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05D3\u05D9\u05D5\u05E7, \u05DC\
  \u05E0\u05D4\u05DC \u05D6\u05D9\u05DB\u05E8\u05D5\u05DF, \u05D0\u05D5 \u05DC\u05D4\
  \u05E4\u05D5\u05DA \u05D0\u05EA \u05D4\u05E4\u05DC\u05D8 \u05DC\u05E0\u05D5\u05D7\
  \ \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 - \u05DB\u05DE\u05D5 \u05DC\u05D4\u05E4\u05D5\
  \u05DA 2.998 \u05DC-3 \u05E0\u05E7\u05D9."
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
עיגול הוא קיצוץ הרעש לאחר נקודה מסוימת במספר. מתכנתים מעגלים כדי לשלוט בדיוק, לנהל זיכרון, או להפוך את הפלט לנוח למשתמש - כמו להפוך 2.998 ל-3 נקי.

## איך לעשות:
הנה איך מעגלים מספרים ב-JavaScript באמצעות `Math.round()`, `Math.ceil()`, ו-`Math.floor()`:

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (מכיוון ש-.567 הוא יותר מ-.5)

console.log(roundedDown); // מדפיס: 2
console.log(roundedUp);   // מדפיס: 3
console.log(rounded);     // מדפיס: 3
```

כדי לתקן למספר מסוים של מקומות עשרוניים, השתמשו ב-`toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (מחזיר מחרוזת)

console.log(twoDecimals); // מדפיס: "2.57"
```

המרו את המחרוזת חזרה למספר באמצעות פלוס אחד או `Number()`:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // מדפיס: 2.57
```

## צלילה עמוקה
עיגול מספרים איננו חידוש; הוא קיים כמו המספרים עצמם. ב-JavaScript, `Math.round()` משתמש בשיטת "עיגול חצי כלפי מעלה": אם חלק העשרוני הוא 0.5, הוא יעגל למספר הזוגי הקרוב ביותר.

לשליטה רבה יותר, `toFixed()` יכול להיות האפשרות המועדפת עליכם, אך זכרו שהוא מחזיר מחרוזת. המרה חזרה למספר יכולה להיות צעד נוסף אך מבטיחה שתמשיכו לעבוד עם סוגים מספריים.

אלטרנטיבות? ספריות כמו `lodash` מציעות `_.round(number, [precision=0])` לשליטה עדינה יותר. או, `Intl.NumberFormat` החדש יותר מספק עיצוב בדיוק גבוה מעבר לעיגול בלבד.

בהתייחסות לדיוק, היזהרו מחריגות שברי הנקודה הצפה ב-JavaScript. `0.1 + 0.2` לא שווה בדיוק ל-`0.3` בגלל אופן אחסון המספרים. לפעמים, עיגול הופך להכרחי כדי לתקן טעויות כאלו.

## ראו גם
- תיעוד ה-Math של Mozilla: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- עיגול פיננסי עם `Intl.NumberFormat`: [ECMAScript Internationalization API](https://tc39.es/ecma402/#numberformat-objects)
- עיגול `lodash`: [Lodash Docs](https://lodash.com/docs/4.17.15#round)
