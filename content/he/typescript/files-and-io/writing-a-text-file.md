---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:07.530053-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: TypeScript \u05E2\
  \u05E6\u05DE\u05D5 \u05DC\u05D0 \u05DE\u05EA\u05DE\u05D5\u05D3\u05D3 \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05D9\u05E9\u05D9\u05E8 \u05E2\u05DD \u05E4\u05E2\u05D5\u05DC\
  \u05D5\u05EA \u05E7\u05D5\u05D1\u05E5 \u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\
  \u05D5\u05D0 \u05DE\u05EA\u05D5\u05E8\u05D2\u05DD \u05DC-JavaScript, \u05E9\u05D1\
  \u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05E4\u05D5\u05E2\u05DC \u05D1\u05D3\u05E4\
  \u05D3\u05E4\u05DF \u05E2\u05DD \u05D2\u05D9\u05E9\u05D4 \u05DE\u05D5\u05D2\u05D1\
  \u05DC\u05EA \u05DC\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\u05D9\
  \u05DD. \u05E2\u05DD \u05D6\u05D0\u05EA,\u2026"
lastmod: '2024-03-13T22:44:38.948208-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u05E2\u05E6\u05DE\u05D5 \u05DC\u05D0 \u05DE\u05EA\u05DE\u05D5\
  \u05D3\u05D3 \u05D1\u05D0\u05D5\u05E4\u05DF \u05D9\u05E9\u05D9\u05E8 \u05E2\u05DD\
  \ \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E7\u05D5\u05D1\u05E5 \u05DB\u05D9\u05D5\
  \u05D5\u05DF \u05E9\u05D4\u05D5\u05D0 \u05DE\u05EA\u05D5\u05E8\u05D2\u05DD \u05DC\
  -JavaScript, \u05E9\u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05E4\u05D5\u05E2\
  \u05DC \u05D1\u05D3\u05E4\u05D3\u05E4\u05DF \u05E2\u05DD \u05D2\u05D9\u05E9\u05D4\
  \ \u05DE\u05D5\u05D2\u05D1\u05DC\u05EA \u05DC\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\
  \u05E7\u05D1\u05E6\u05D9\u05DD."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## איך לעשות:
TypeScript עצמו לא מתמודד באופן ישיר עם פעולות קובץ כיוון שהוא מתורגם ל-JavaScript, שבדרך כלל פועל בדפדפן עם גישה מוגבלת למערכת הקבצים. עם זאת, כאשר משתמשים בו בסביבת Node.js, המודול `fs`‏ (מערכת קבצים) מספק פונקציונליות לכתיבת קבצים.

### שימוש במודול fs של Node.js
ראשית, ודא שאתה פועל בסביבת Node.js. לאחר מכן, השתמש במודול `fs` כדי לכתוב קבצי טקסט. הנה דוגמה בסיסית:

```typescript
import * as fs from 'fs';

const data = 'שלום, עולם!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('הקובץ נשמר!');
});
```

זה יכתוב באופן אסינכרוני "שלום, עולם!" ל-`message.txt`. אם הקובץ אינו קיים, Node.js יוצר אותו; אם הוא כן קיים, Node.js יחליף אותו.

לכתיבת קובץ באופן סינכרוני, השתמש ב-`writeFileSync`:

```typescript
import * as fs from 'fs';

const data = 'שוב שלום, עולם!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('הקובץ נשמר!');
} catch (err) {
    console.error(err);
}
```

### שימוש בספריות צד שלישי פופולריות
למרות שהמודול `fs` המקורי חזק, חלק מהמפתחים מעדיפים להשתמש בספריות צד שלישי לנוחות ופונקציונליות נוספות. `fs-extra` היא בחירה פופולרית אשר מרחיבה את `fs` והופכת את פעולות הקבצים לפשוטות יותר.

ראשית, תצטרך להתקין את `fs-extra`:

```
npm install fs-extra
```

לאחר מכן, תוכל להשתמש בו בקובץ ה-TypeScript שלך לכתיבת תוכן טקסט:

```typescript
import * as fs from 'fs-extra';

const data = 'זהו fs-extra!';
const filePath = './extraMessage.txt';

// באמצעות async/await
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('הקובץ נשמר עם fs-extra!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

הדוגמה הזו עושה את אותו הדבר כמו הדוגמאות הקודמות עם `fs` אך משתמשת בספריית `fs-extra`, מה שמציע תחביר נקי יותר לטיפול ב-promises.
