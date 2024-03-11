---
date: 2024-01-26 03:47:48.541648-07:00
description: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05D4\u05D5\u05D0 \u05D2\u05D6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8 \u05DC\
  \u05D3\u05D9\u05D5\u05E7 \u05DE\u05E1\u05D5\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\
  \u05D9 \u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05E4\u05DC\u05D8 \u05DE\u05E1\u05E4\
  \u05E8\u05D9 \u05DC\u05E6\u05D5\u05E8\u05DA \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA\
  , \u05EA\u05E6\u05D5\u05D2\u05D4 \u05D0\u05D5 \u05DB\u05D0\u05E9\u05E8 \u05D3\u05D9\
  \u05D5\u05E7 \u05DE\u05E1\u05D5\u05D9\u05DD \u05E0\u05D3\u05E8\u05E9 \u05DC\u05D0\
  \u05D7\u05E8 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E9\u05DE\u05E0\u05D9\u05D1\
  \u05D5\u05EA \u05EA\u05D5\u05E6\u05D0\u05D5\u05EA \u05E9\u05D1\u05E8\u2026"
lastmod: '2024-03-11T00:14:12.340607-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D4\
  \u05D5\u05D0 \u05D2\u05D6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8 \u05DC\u05D3\
  \u05D9\u05D5\u05E7 \u05DE\u05E1\u05D5\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05E4\u05DC\u05D8 \u05DE\u05E1\u05E4\u05E8\
  \u05D9 \u05DC\u05E6\u05D5\u05E8\u05DA \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA, \u05EA\
  \u05E6\u05D5\u05D2\u05D4 \u05D0\u05D5 \u05DB\u05D0\u05E9\u05E8 \u05D3\u05D9\u05D5\
  \u05E7 \u05DE\u05E1\u05D5\u05D9\u05DD \u05E0\u05D3\u05E8\u05E9 \u05DC\u05D0\u05D7\
  \u05E8 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E9\u05DE\u05E0\u05D9\u05D1\u05D5\
  \u05EA \u05EA\u05D5\u05E6\u05D0\u05D5\u05EA \u05E9\u05D1\u05E8\u2026"
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
עיגול מספרים הוא גזירת מספר לדיוק מסוים. מתכנתים עושים זאת כדי לשלוט בפלט מספרי לצורך קריאות, תצוגה או כאשר דיוק מסוים נדרש לאחר פעולות שמניבות תוצאות שבר צפה.

## איך לעשות:
עיגול ב-TypeScript ניתן לבצע באמצעות מספר שיטות. הנה סקירה מהירה:

```typescript
// Math.round מעגל לשלם הקרוב ביותר
console.log(Math.round(1.5)); // פלט: 2

// Math.ceil מעגל למעלה לשלם הקרוב ביותר
console.log(Math.ceil(1.1)); // פלט: 2

// Math.floor מעגל למטה לשלם הקרוב ביותר
console.log(Math.floor(1.8)); // פלט: 1

// toFixed מעגל למספר קבוע של מקומות עשרוניים
let num = 1.23456;
console.log(num.toFixed(2)); // פלט: "1.23"
// שים לב: toFixed מחזיר מחרוזת! השתמש בparseFloat להמרה חזרה אם נדרש.
console.log(parseFloat(num.toFixed(2))); // פלט: 1.23
```

## נפח עמוק
בימים ימימה, עיגול היה הכרחי בגלל מקום מוגבל ובעיות דיוק במחשבים המוקדמים. כיום, חישובים עם שברים צפים יכולים להוביל לתוצאות מוזרות בגלל אופן אחסון המספרים בבינארי. חלופות לעיגול כוללות floor, ceil, ו-trunc (לגזירת עשרוניים ללא עיגול).

חשוב להתייחס לפנימיים: `Math.round` עוקב אחרי "עיגול חצי למעלה" (גם "עיגול מסחרי"), בעוד `Math.floor` ו-`Math.ceil` פשוטים. `toFixed` עלול לגרום לתוצאות בלתי צפויות מכיוון שהוא מחזיר מחרוזת, והוא מעגל בשיטת "עיגול חצי לזוגי" (גם "עיגול בנקאי"), במיוחד שימושי להקטנת הטיה בעיגול אותם מספרים מספר פעמים.

## ראה גם
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [התקן IEEE לחישוב צפה (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
