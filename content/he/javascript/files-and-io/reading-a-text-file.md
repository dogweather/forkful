---
title:                "קריאת קובץ טקסט"
aliases:
- /he/javascript/reading-a-text-file/
date:                  2024-01-20T17:55:13.009204-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/reading-a-text-file.md"
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
