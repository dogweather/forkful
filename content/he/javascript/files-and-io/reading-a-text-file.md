---
date: 2024-01-20 17:55:13.009204-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-JavaScript \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D2\
  \u05E9\u05EA \u05DC\u05EA\u05D5\u05DB\u05DF \u05E9\u05DE\u05D0\u05D5\u05D7\u05E1\
  \u05DF \u05D1\u05E7\u05D5\u05D1\u05E5 \u05DE\u05E1\u05D5\u05D9\u05DD. \u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05DC\u05D8\u05E2\u05D5\u05DF \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA, \u05D0\u05D5\
  \ \u05DC\u05E7\u05E8\u05D5\u05D0 \u05DE\u05D9\u05D3\u05E2 \u05D3\u05D9\u05E0\u05D0\
  \u05DE\u05D9 \u05DE\u05E7\u05D5\u05D1\u05E5."
lastmod: '2024-03-11T00:14:13.501389-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-JavaScript \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D2\u05E9\
  \u05EA \u05DC\u05EA\u05D5\u05DB\u05DF \u05E9\u05DE\u05D0\u05D5\u05D7\u05E1\u05DF\
  \ \u05D1\u05E7\u05D5\u05D1\u05E5 \u05DE\u05E1\u05D5\u05D9\u05DD. \u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\
  \u05D8\u05E2\u05D5\u05DF \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA, \u05D0\u05D5 \u05DC\
  \u05E7\u05E8\u05D5\u05D0 \u05DE\u05D9\u05D3\u05E2 \u05D3\u05D9\u05E0\u05D0\u05DE\
  \u05D9 \u05DE\u05E7\u05D5\u05D1\u05E5."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט ב-JavaScript זה פשוט לגשת לתוכן שמאוחסן בקובץ מסוים. תכנתים עושים את זה כדי לעבד נתונים, לטעון הגדרות, או לקרוא מידע דינאמי מקובץ.

## איך לעשות:
בואו נראה קוד מדגם: נשתמש ב-Node.js כי בדפדפן זה קצת יותר מסובך מסיבות של אבטחת מידע.

```javascript
const fs = require('fs');

fs.readFile('example.txt', 'utf8', (err, data) => {
  if (err) {
    console.error('Error reading the file:', err);
    return;
  }
  console.log(data);
});
```

אם הקובץ example.txt יכיל את הטקסט "שלום עולם", הפלט יהיה:
```
שלום עולם
```

## עיון מעמיק:
בעבר, ביצענו קריאת קבצים באופן סינכרוני - הואמנו את הפעולה לסיים לפני שהמערכת תמשיך לתהליך הבא. זה גרם לבלוק של התוכנית. עם Node.js, אנחנו מעדיפים בדרך כלל פעולות אסינכרוניות כדי לשפר את היעילות.

דבר שני, אל תשכח את האופציה לשימוש ב-`readFileSync` אם אתה רוצה קריאה סינכרונית:

```javascript
const data = fs.readFileSync('example.txt', 'utf8');
console.log(data);
```

והיום, אפשר גם להשתמש ב-`async/await` עם promises לקריאת קבצים בצורה אסינכרונית קלאסית:

```javascript
const fsPromises = require('fs').promises;

async function readFile() {
  try {
    const data = await fsPromises.readFile('example.txt', 'utf8');
    console.log(data);
  } catch (err) {
    console.error('Error reading the file:', err);
  }
}

readFile();
```

עוד עניין לזכור - במערכות גדולות, כדאי לשקול לקרוא בזרימה (streams), כדי להוריד את שימוש בזיכרון.

## ראו גם:
למידע נוסף ולהרחבה על קריאת קבצים בNode.js, עיינו במקורות הבאים:

- [Node.js fs.readFile documentation](https://nodejs.org/api/fs.html#fsreadfilepath-options-callback)
- [Node.js fs.readFileSync documentation](https://nodejs.org/api/fs.html#fsreadfilesyncpath-options)
- [Working with file streams in Node.js](https://nodejs.org/api/stream.html)
