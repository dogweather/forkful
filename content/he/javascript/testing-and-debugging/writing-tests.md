---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:52.492095-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05D1-JavaScript \u05DE\u05EA\u05D9\u05D9\u05D7\u05E1\u05EA \u05DC\u05EA\u05D9\
  \u05DE\u05E8\u05D5\u05DF \u05E9\u05DC \u05D9\u05E6\u05D9\u05E8\u05EA \u05E1\u05E7\
  \u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05D9\
  \u05DD \u05E9\u05DE\u05E8\u05D9\u05E6\u05D9\u05DD \u05D0\u05EA \u05D4\u05E7\u05D5\
  \u05D3 \u05E9\u05DC\u05DA \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\
  \u05D4\u05D5\u05D0 \u05DE\u05EA\u05E0\u05D4\u05D2 \u05DB\u05E6\u05E4\u05D5\u05D9\
  , \u05D3\u05D1\u05E8 \u05E9\u05D9\u05DB\u05D5\u05DC \u05DC\u05E9\u05E4\u05E8 \u05DE\
  \u05E9\u05DE\u05E2\u05D5\u05EA\u05D9\u05EA \u05D0\u05EA \u05D0\u05DE\u05D9\u05E0\
  \u05D5\u05EA\u05DD\u2026"
lastmod: '2024-03-11T00:14:13.477404-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\
  -JavaScript \u05DE\u05EA\u05D9\u05D9\u05D7\u05E1\u05EA \u05DC\u05EA\u05D9\u05DE\u05E8\
  \u05D5\u05DF \u05E9\u05DC \u05D9\u05E6\u05D9\u05E8\u05EA \u05E1\u05E7\u05E8\u05D9\
  \u05E4\u05D8\u05D9\u05DD \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05D9\u05DD\
  \ \u05E9\u05DE\u05E8\u05D9\u05E6\u05D9\u05DD \u05D0\u05EA \u05D4\u05E7\u05D5\u05D3\
  \ \u05E9\u05DC\u05DA \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\
  \u05D5\u05D0 \u05DE\u05EA\u05E0\u05D4\u05D2 \u05DB\u05E6\u05E4\u05D5\u05D9, \u05D3\
  \u05D1\u05E8 \u05E9\u05D9\u05DB\u05D5\u05DC \u05DC\u05E9\u05E4\u05E8 \u05DE\u05E9\
  \u05DE\u05E2\u05D5\u05EA\u05D9\u05EA \u05D0\u05EA \u05D0\u05DE\u05D9\u05E0\u05D5\
  \u05EA\u05DD\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת בדיקות ב-JavaScript מתייחסת לתימרון של יצירת סקריפטים אוטומטיים שמריצים את הקוד שלך כדי לוודא שהוא מתנהג כצפוי, דבר שיכול לשפר משמעותית את אמינותם וניתנותם לתחזוקה של היישומים שלך. מתכנתים עושים זאת כדי לתפוס תקלות בשלב מוקדם, להקל על שינוי מבנה של קוד, ולוודא שתכונות חדשות לא ישברו פונקציונליות קיימות.

## איך לעשות:

### גישה טבעית (באמצעות Jest)

Jest הוא מסגרת בדיקות פופולרית שמספקת API ידידותי לכתיבת בדיקות יחידה ב-JavaScript. היא דורשת מעט הגדרה ומגיעה עם תכונות כמו פונקציות מדמה, טיימרים ובדיקת Snapshot.

1. **התקנה**:

```bash
npm install --save-dev jest
```

2. **כתיבת בדיקה פשוטה**:

יצירת קובץ בשם `sum.test.js`:

```javascript
const sum = require('./sum'); // נניח שפונקציה זו פשוט מחברת שני מספרים

test('מוסיף 1 + 2 לקבלת 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **הרצת הבדיקה שלך**:

```bash
npx jest
```

**פלט לדוגמא:**

```plaintext
PASS  ./sum.test.js
✓ מוסיף 1 + 2 לקבלת 3 (5ms)
```

### בדיקת קוד אסינכרוני

Jest מקל על בדיקת promises ותחביר async/await:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('חיבור אסינכרוני עובד', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### שימוש בספריות צד שלישי (Mocha & Chai)

Mocha היא מסגרת בדיקות פופולרית נוספת, שלעיתים קרובות משמשת יחד עם ספריית ההבטחות Chai לבדיקות יותר אקספרסיביות.

1. **התקנה**:

```bash
npm install --save-dev mocha chai
```

2. **כתיבת בדיקה עם Mocha ו-Chai**:

יצירת `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // מודול חישוב פשוט

describe('Calculate', function() {
  it('אמור לחבר שני ערכים', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **הרצת הבדיקות שלך עם Mocha**:

הוסף סקריפט ב-`package.json` שלך:

```json
"scripts": {
  "test": "mocha"
}
```

לאחר מכן הרץ:

```bash
npm test
```

**פלט לדוגמא:**

```plaintext
  Calculate
    ✓ אמור לחבר שני ערכים


  1 passing (8ms)
```

דוגמאות אלו ממחישות כתיבת והרצת בדיקות בסיסית ב-JavaScript. אימוץ מסגרת בדיקות כמו Jest או Mocha יחד עם Chai יכול לספק בסיס מוצק לבדיקות יישום עמידות, שעוזר לוודא שהקוד שלך מתפקד כצפוי לאורך עדכונים ושינויי מבנה.
