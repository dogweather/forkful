---
title:                "הדפסת פלט לניפוי באגים"
date:                  2024-01-20T17:54:06.056103-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"

category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
רשימות דיבוג (Debug output) הן פשוט הודעות שמתכנתים מוסיפים לקוד כדי לעקוב אחרי מה שקורה. אנחנו עושים את זה כדי להבין במהירות איפה הבעיות ואיך הקוד שלנו עובד.

## איך לעשות:
```TypeScript
// הדפסת פלט פשוט לקונסול
console.log('זה פלט דיבוג');

// דוגמה לשילוב משתנים בהדפסה
let user = 'משה';
console.log(`שלום ${user}, זה הלוג שלך`);

// שימוש ב console.error ו console.warn להראות שגיאות או אזהרות
console.error('משהו השתבש!');
console.warn('זהירות, יש בעיה פוטנציאלית.');

// דוגמה להדפסת אובייקטים מורכבים
let userObj = { name: 'משה', age: 30 };
console.log(userObj);

// פלט דוגמה
// זה פלט דיבוג
// שלום משה, זה הלוג שלך
// משהו השתבש!
// זהירות, יש בעיה פוטנציאלית.
// { name: 'משה', age: 30 }
```

## צלילה עמוקה
אחת הדרכים הראשונות לדיבוג קוד הייתה להוסיף הדפסות לקונסול, וזה עדיין כלי יעיל. ישנן גם אלטרנטיבות כמו מתקנים לניטור קוד (profilers) וכלי דיבוג מובנים (integrated debuggers) שמציעים מידע עמוק יותר. בTypescript, `console.log` וכדומה משתמשים ביכולת הבנויה של מנועי Javascript בדפדפנים וסביבות הרצה כמו Node.js להדפיס לקונסול - אבל אל תשכחו, בקוד לפרודקשן כדאי לשלוט על זה ולא להדפיס יותר מדי.

## ראו גם
- [המדריך הרשמי ל-TypeScript](https://www.typescriptlang.org/docs/)
- [תיעוד על `console` ב-MDN](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Node.js דוקומנטציה על קונסול](https://nodejs.org/api/console.html)
- [המדריך ל-Node.js Debugging](https://nodejs.org/en/docs/guides/debugging-getting-started/)
