---
date: 2024-01-26 00:55:32.603734-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E0\u05D4\
  \ \u05D1\u05DC\u05D5\u05E7 \u05D4-`try-catch` \u05D4\u05E7\u05DC\u05D0\u05E1\u05D9\
  ."
lastmod: '2024-03-13T22:44:39.987598-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E0\u05D4 \u05D1\u05DC\u05D5\u05E7 \u05D4-`try-catch` \u05D4\u05E7\
  \u05DC\u05D0\u05E1\u05D9."
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 16
---

## איך לעשות:
הנה בלוק ה-`try-catch` הקלאסי:

```javascript
try {
  // קוד שעלול לזרוק שגיאה
  let result = potentiallyRiskyOperation();
  console.log('הצלחה:', result);
} catch (error) {
  // מה לעשות אם נזרקת שגיאה
  console.error('אופס:', error.message);
}
```

פלט לדוגמא כאשר לא מתרחשת שגיאה:
```
הצלחה: 42
```

וכאשר יש שגיאה:
```
אופס: משהו השתבש
```

עבור קוד אסינכרוני, שבו מעורבים promises, השתמשו ב-`try-catch` בפונקציה `async`:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('נתונים נאספו:', data);
  } catch (error) {
    console.error('שגיאה באיסוף נתונים:', error.message);
  }
}

fetchData();
```

## צלילה עמוקה
טיפול בשגיאות ב-JavaScript התפתח. בעבר (ES3, בערך 1999), היינו כוללים רק את בלוק ה-`try-catch`. לא ממש גמיש, אבל עשה את העבודה.

ES6 (2015) הציג Promises ונתן לנו את השיטות `.then()` ו-`.catch()`, שמאפשרים לנו לטפל בשגיאות אסינכרוניות בצורה חלקה יותר.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('נתונים נאספו:', data))
  .catch(error => console.error('שגיאה באיסוף נתונים:', error.message));
```

לגבי פרטי המימוש, כאשר שגיאה נזרקת, מנועי JavaScript יוצרים אובייקט של `Error` עם מאפיינים שימושיים כמו `message` ו-`stack`. אפשר גם ליצור סוגי שגיאה מותאמים אישית על ידי הרחבת המחלקה `Error` – נוח לאפליקציות מורכבות יותר.

אלטרנטיבות? תוכלו להתעלם מטיפול בשגיאות (רעיון רע), להשתמש ב-callbacks עם פרמטרים של שגיאה-ראשונה (שלום, סגנון Node.js), או להשיג יופי עם ספריות ומסגרות שמציעות את הגישות שלהן.

## ראה גם
למידע נוסף על טיפול בשגיאות:

- MDN על try-catch: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- מדריך ל-Promises: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- יצירה וזריקה של שגיאות מותאמות אישית: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
