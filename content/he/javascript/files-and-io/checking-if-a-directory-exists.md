---
aliases:
- /he/javascript/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:12.462012-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-JavaScript \u05D7\u05E9\u05D5\u05D1\
  \u05D4 \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05E9\u05DC \u05E2\u05D9\u05D1\
  \u05D5\u05D3 \u05E7\u05D1\u05E6\u05D9\u05DD, \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA\
  \ \u05DC\u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05DC\u05D5\u05D5\u05D3\
  \u05D0 \u05D0\u05EA \u05E0\u05D5\u05DB\u05D7\u05D5\u05EA \u05D4\u05E1\u05E4\u05E8\
  \u05D9\u05D4 \u05DC\u05E4\u05E0\u05D9 \u05D4\u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\
  \u05DE\u05E0\u05D4 \u05D0\u05D5 \u05D4\u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\
  \u05D9\u05D4. \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05DE\u05D5\u05E0\u05E2\
  \u05EA\u2026"
lastmod: 2024-02-18 23:08:53.263490
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D4\
  \ \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-JavaScript \u05D7\u05E9\u05D5\u05D1\u05D4\
  \ \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05E9\u05DC \u05E2\u05D9\u05D1\u05D5\
  \u05D3 \u05E7\u05D1\u05E6\u05D9\u05DD, \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\
  \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05DC\u05D5\u05D5\u05D3\u05D0\
  \ \u05D0\u05EA \u05E0\u05D5\u05DB\u05D7\u05D5\u05EA \u05D4\u05E1\u05E4\u05E8\u05D9\
  \u05D4 \u05DC\u05E4\u05E0\u05D9 \u05D4\u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05DE\
  \u05E0\u05D4 \u05D0\u05D5 \u05D4\u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\
  \u05D4. \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05DE\u05D5\u05E0\u05E2\u05EA\
  \u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספריה קיימת ב-JavaScript חשובה למשימות של עיבוד קבצים, מאפשרת לסקריפטים לוודא את נוכחות הספריה לפני הקריאה ממנה או הכתיבה אליה. פעולה זו מונעת שגיאות ומבטיחה ביצוע חלק יותר של התוכנית, במיוחד ביישומים המטפלים בצורה דינמית בקבצים או בספריות בהתבסס על קלט מהמשתמש או ממקורות נתונים חיצוניים.

## איך לעשות זאת:
ב-Node.js, מאחר של-JavaScript עצמו אין גישה ישירה למערכת הקבצים, מודול ה-`fs` משמש לרוב לפעולות מסוג זה. הנה דרך פשוטה לבדוק אם ספריה קיימת באמצעות `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// בדיקה אם הספריה קיימת
if (fs.existsSync(directoryPath)) {
  console.log('הספריה קיימת.');
} else {
  console.log('הספריה לא קיימת.');
}
```
**פלט לדוגמה:**
```
הספריה קיימת.
```
או, לגישה אסינכרונית ללא חסימה, השתמשו ב-`fs.promises` עם `async/await`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('הספריה קיימת.');
  } catch (error) {
    console.log('הספריה לא קיימת.');
  }
}

checkDirectory('./sample-directory');
```
**פלט לדוגמה:**
```
הספריה קיימת.
```

לפרויקטים המשתמשים בצורה נרחבת בפעולות של קבצים וספריות, החבילה `fs-extra`, שהיא הרחבה של מודול ה-`fs` המקורי, מציעה שיטות נוספות נוחות. הנה איך אפשר לעשות את אותו הדבר עם `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// בדיקה אם הספריה קיימת
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'הספריה קיימת.' : 'הספריה לא קיימת.'))
  .catch(err => console.error(err));
```
**פלט לדוגמה:**
```
הספריה קיימת.
```

גישה זו מאפשרת קוד נקי, קריא שמשתלב בצורה חלקה עם המתודות המודרניות של JavaScript.
