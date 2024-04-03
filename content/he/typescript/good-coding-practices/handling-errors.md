---
date: 2024-01-26 00:59:02.840144-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05B0: \u05D1-TypeScript, \u05D8\u05D9\u05E4\
  \u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05DC\u05E2\u05D9\u05EA\
  \u05D9\u05DD \u05DB\u05E8\u05D5\u05DA \u05D1\u05D1\u05DC\u05D5\u05E7\u05D9\u05DD\
  \ \u05E9\u05DC `try`, `catch`, \u05D5-`finally`."
lastmod: '2024-03-13T22:44:38.929814-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-TypeScript, \u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\
  \u05D0\u05D5\u05EA \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05DB\u05E8\u05D5\u05DA\
  \ \u05D1\u05D1\u05DC\u05D5\u05E7\u05D9\u05DD \u05E9\u05DC `try`, `catch`, \u05D5\
  -`finally`."
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 16
---

## איך לְ:
ב-TypeScript, טיפול בשגיאות לעיתים כרוך בבלוקים של `try`, `catch`, ו-`finally`.

```typescript
function riskyOperation() {
  throw new Error("משהו השתבש!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("תפסתי שגיאה:", error.message);
  } finally {
    console.log("זה תמיד רץ, אם יש שגיאה או לא.");
  }
}

handleErrors();
```

פלט לדוגמה:

```
תפסתי שגיאה: משהו השתבש!
זה תמיד רץ, אם יש שגיאה או לא.
```

דוגמה אסינכרונית עם הבטחות (promises):

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // סימולציה של שגיאה
    reject("נכשל באופן חמור");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("תפסתי שגיאה אסינכרונית:", error);
  }
}

handleAsyncErrors();
```

פלט לדוגמה:

```
תפסתי שגיאה אסינכרונית: נכשל באופן חמור
```

## עיון נוסף
טיפול בשגיאות היה אבן דרך בתוכנות מאז היווסדותה. ב-TypeScript, שמפתח על גבי JavaScript, טיפול בשגיאות הפך ליותר עמיד עם ההכנסה של async/await ב-ECMAScript 2017. לפני כן, לעיתים נסמכנו על פונקציות קולבק והבטחות כדי לטפל בשגיאות בקוד אסינכרוני.

חלופה ל`try/catch` ב-TypeScript היא שימוש בגבולות שגיאה שמספקים פריימוורקים כמו React. לטיפול בצד-השרת, אנו יכולים להשתמש בתוכנת ביניים (middleware) בפלטפורמות כמו Express.js למרכז ניהול שגיאות.

מבחינת יישום, ב-TypeScript אין מנגנון טיפול בשגיאות משלו אלא הוא מסתמך על זה של JavaScript. מחלקות שגיאה מותאמות אישית יכולות לירש את המחלקה `Error` כדי לספק מידע תיאורי יותר על שגיאות.

## ראה גם
- [MDN על try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await ב-MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [שימוש בגבולות שגיאה ב-React](https://reactjs.org/docs/error-boundaries.html)
- [טיפול בשגיאות ב-Express.js](https://expressjs.com/en/guide/error-handling.html)
