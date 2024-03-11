---
date: 2024-01-26 00:59:02.840144-07:00
description: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05E2\u05D5\u05E1\u05E7 \u05D1\u05E6\u05D9\u05E4\u05D9\u05D9\u05D4 \u05DC\u05DC\
  \u05D0 \u05E6\u05E4\u05D5\u05D9; \u05D6\u05D4 \u05DB\u05D9\u05E6\u05D3 \u05D0\u05E0\
  \u05D5 \u05DE\u05E0\u05D4\u05DC\u05D9\u05DD \u05DE\u05E6\u05D1\u05D9\u05DD \u05D1\
  \u05D4\u05DD \u05D3\u05D1\u05E8\u05D9\u05DD \u05D4\u05D5\u05DC\u05DB\u05D9\u05DD\
  \ \u05DC\u05D0 \u05DB\u05E4\u05D9 \u05E9\u05EA\u05D5\u05DB\u05E0\u05E0\u05D5 \u05D1\
  \u05E7\u05D5\u05D3 \u05E9\u05DC\u05E0\u05D5. \u05D0\u05E0\u05D5 \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05DE\u05E0\u05D5\u05E2\
  \ \u05E7\u05E8\u05D9\u05E1\u05D5\u05EA \u05D5\u05DC\u05EA\u05EA \u05DC\u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD\u2026"
lastmod: '2024-03-11T00:14:12.361968-06:00'
model: gpt-4-1106-preview
summary: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05E2\u05D5\u05E1\u05E7 \u05D1\u05E6\u05D9\u05E4\u05D9\u05D9\u05D4 \u05DC\u05DC\
  \u05D0 \u05E6\u05E4\u05D5\u05D9; \u05D6\u05D4 \u05DB\u05D9\u05E6\u05D3 \u05D0\u05E0\
  \u05D5 \u05DE\u05E0\u05D4\u05DC\u05D9\u05DD \u05DE\u05E6\u05D1\u05D9\u05DD \u05D1\
  \u05D4\u05DD \u05D3\u05D1\u05E8\u05D9\u05DD \u05D4\u05D5\u05DC\u05DB\u05D9\u05DD\
  \ \u05DC\u05D0 \u05DB\u05E4\u05D9 \u05E9\u05EA\u05D5\u05DB\u05E0\u05E0\u05D5 \u05D1\
  \u05E7\u05D5\u05D3 \u05E9\u05DC\u05E0\u05D5. \u05D0\u05E0\u05D5 \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05DE\u05E0\u05D5\u05E2\
  \ \u05E7\u05E8\u05D9\u05E1\u05D5\u05EA \u05D5\u05DC\u05EA\u05EA \u05DC\u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD\u2026"
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
טיפול בשגיאות עוסק בציפייה ללא צפוי; זה כיצד אנו מנהלים מצבים בהם דברים הולכים לא כפי שתוכננו בקוד שלנו. אנו עושים זאת כדי למנוע קריסות ולתת למשתמשים חוויה חלקה, גם כאשר קורה הלא צפוי.

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
