---
date: 2024-01-20 17:54:06.056103-07:00
description: "\u05E8\u05E9\u05D9\u05DE\u05D5\u05EA \u05D3\u05D9\u05D1\u05D5\u05D2\
  \ (Debug output) \u05D4\u05DF \u05E4\u05E9\u05D5\u05D8 \u05D4\u05D5\u05D3\u05E2\u05D5\
  \u05EA \u05E9\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D5\u05E1\u05D9\
  \u05E4\u05D9\u05DD \u05DC\u05E7\u05D5\u05D3 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E7\
  \u05D5\u05D1 \u05D0\u05D7\u05E8\u05D9 \u05DE\u05D4 \u05E9\u05E7\u05D5\u05E8\u05D4\
  . \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D9\u05DF \u05D1\u05DE\u05D4\u05D9\
  \u05E8\u05D5\u05EA \u05D0\u05D9\u05E4\u05D4 \u05D4\u05D1\u05E2\u05D9\u05D5\u05EA\
  \ \u05D5\u05D0\u05D9\u05DA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05E0\u05D5\u2026"
lastmod: '2024-03-13T22:44:38.921715-06:00'
model: gpt-4-1106-preview
summary: "\u05E8\u05E9\u05D9\u05DE\u05D5\u05EA \u05D3\u05D9\u05D1\u05D5\u05D2 (Debug\
  \ output) \u05D4\u05DF \u05E4\u05E9\u05D5\u05D8 \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA\
  \ \u05E9\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D5\u05E1\u05D9\u05E4\
  \u05D9\u05DD \u05DC\u05E7\u05D5\u05D3 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E7\u05D5\
  \u05D1 \u05D0\u05D7\u05E8\u05D9 \u05DE\u05D4 \u05E9\u05E7\u05D5\u05E8\u05D4. \u05D0\
  \u05E0\u05D7\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D9\u05DF \u05D1\u05DE\u05D4\u05D9\u05E8\
  \u05D5\u05EA \u05D0\u05D9\u05E4\u05D4 \u05D4\u05D1\u05E2\u05D9\u05D5\u05EA \u05D5\
  \u05D0\u05D9\u05DA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05E0\u05D5\u2026"
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
weight: 33
---

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
