---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:50:18.594502-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים היא שיטה לייצר מספרים שאין בהם דפוס צפוי. תכנות זורק מספרים אקראיים כדי להוסיף אקראיות ותחושה של חדשנות במשחקים, בחישובים סטטיסטיים ובמקרים בהם דרוש אלמנט של בלתי צפויות.

## איך לעשות:
TypeScript מספק דרך פשוטה ליצירת מספר אקראי באמצעות `Math.random()`. עם זאת, זה יוצר רק מספרים בין 0 ל-1. להלן דוגמאות של כיצד להשתמש בזה כדי לייצר מספרים אקראיים בטווחים שונים:

```TypeScript
// יצירת מספר אקראי בין 0 ל-1
let randomNum = Math.random();
console.log(randomNum);

// יצירת מספר אקראי בין 1 ל-10
let randomBetweenOneAndTen = Math.floor(Math.random() * 10) + 1;
console.log(randomBetweenOneAndTen);

// יצירת מספר אקראי בין מספרים שאתם בוחרים
let min = 50;
let max = 100;
let randomBetweenMinAndMax = Math.floor(Math.random() * (max - min + 1)) + min;
console.log(randomBetweenMinAndMax);
```

פלט לדוגמא:
```
0.4371290562345215
7
78
```

## צלילה לעומק
`Math.random()` היא פונקציה מובנית ב-JavaScript ומשמשת גם ב-TypeScript ליצירת מספרים פסאודו-אקראיים. המספרים שמיוצרים הם לא אקראיים באופן אבסולוטי, אלא נראים אקראיים למעשה. בשנים המוקדמות של מדעי המחשב, יצירת מספרים אקראיים אמיתיים הייתה אתגר, ופותחו אלגוריתמים שונים כדי להתמודד עם זה. במקרים בהם נדרשת חוזק אקראיות גבוה יותר, כמו בקריפטוגרפיה, יש שימוש בגנרטורים קריפטוגרפיים אקראיים. 

ניתן להשתמש ב־Web Crypto API בדפדפנים התומכים בזה לייצר חוזק אקראיות גבוה יותר. כמו כן, ספריות צד שלישי כמו `crypto-js` יכולות לעזור לייצר מספרים אקראיים בטווח רחב יותר של תחום ודרישות.

## קישורים לקריאה נוספת
- אקראיות פסאודו ואקראיות אמיתית:
  [MDN Web Docs על Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- גנרטורים קריפטוגרפיים:
  [MDN Web Docs על Crypto.getRandomValues()](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
- ספריות צד שלישי:
  [crypto-js על GitHub](https://github.com/brix/crypto-js)