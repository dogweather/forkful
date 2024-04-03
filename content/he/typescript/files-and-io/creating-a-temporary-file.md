---
date: 2024-01-20 17:41:23.719065-07:00
description: "\u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05E7\u05D5\u05D1\u05E5 \u05D6\
  \u05DE\u05E0\u05D9 \u05DB\u05D0\u05E9\u05E8 \u05E6\u05E8\u05D9\u05DA \u05DC\u05E9\
  \u05DE\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E2\u05D1\u05D5\u05E8\
  \ \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05D1\u05D9\u05E0\u05D9\u05D9\u05DD \u05D5\
  \u05DC\u05D0 \u05DC\u05D8\u05D5\u05D5\u05D7 \u05D4\u05D0\u05E8\u05D5\u05DA. \u05D6\
  \u05D4 \u05E2\u05D5\u05D6\u05E8 \u05DC\u05D4\u05E4\u05D7\u05D9\u05EA \u05D0\u05EA\
  \ \u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D6\u05D9\u05DB\u05E8\u05D5\u05DF\
  \ \u05D5\u05DC\u05DE\u05E0\u05D5\u05E2 \u05D3\u05DC\u05D9\u05E4\u05D5\u05EA \u05DE\
  \u05D9\u05D3\u05E2 \u05DB\u05E9\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D7\u05D5\u05DC\u05E3 \u05D1\u05DC\u05D1\u05D3."
lastmod: '2024-03-13T22:44:38.949913-06:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05DB\u05D0\u05E9\u05E8 \u05E6\u05E8\u05D9\u05DA \u05DC\u05E9\u05DE\
  \u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E2\u05D1\u05D5\u05E8 \u05E4\
  \u05E2\u05D5\u05DC\u05D5\u05EA \u05D1\u05D9\u05E0\u05D9\u05D9\u05DD \u05D5\u05DC\
  \u05D0 \u05DC\u05D8\u05D5\u05D5\u05D7 \u05D4\u05D0\u05E8\u05D5\u05DA."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

## מה ולמה?
יוצרים קובץ זמני כאשר צריך לשמור נתונים עבור פעולות ביניים ולא לטווח הארוך. זה עוזר להפחית את השימוש בזיכרון ולמנוע דליפות מידע כשטיפול בנתונים חולף בלבד.

## איך לעשות:
נעשה שימוש בספריית `fs` של Node.js בצירוף ההרחבה `promises` למניפולציית קבצים בסגנון אסינכרוני, יחד עם `os` לעבודה עם מערכת ההפעלה.

```typescript
import { promises as fs } from 'fs';
import * as os from 'os';
import * as path from 'path';

async function createTempFile(prefix: string): Promise<string> {
  const tempDir = os.tmpdir();
  const tempFileName = `${prefix}${Date.now()}`;
  const tempFilePath = path.join(tempDir, tempFileName);

  await fs.writeFile(tempFilePath, 'Temporary data');
  console.log(`Created a temp file at: ${tempFilePath}`);
  
  return tempFilePath;
}

createTempFile('myApp_').then((filePath) => {
  // Use your temp file here
});
```

פלט שלהלן יוצג בקונסול:
```
Created a temp file at: C:\Users\<Username>\AppData\Local\Temp\myApp_1612273925396
```

## עיון מעמיק
בעבר, קבצים זמניים היו נוצרים ידנית, עם סיכון לכך שלא יימחקו ויצברו במערכת. ספריות כמו `tmp` ו`temp` ב-NPM מספקות יכולת ליצירה אוטומטית ולניהול טוב יותר של קבצים אלה. ואילו `os.tmpdir()` ו`fs` הם מעניקים שליטה בסיסית וישירה, אבל דורשים טוב יותר ניהול זהירות מצד המתכנת. השימוש בקבצים זמניים חשוב במיוחד באפליקציות שעובדות עם נתונים רגישים או כאשר מטרת השימוש חד פעמית.

## ראה גם
- [Node.js fs module documentation](https://nodejs.org/api/fs.html)
- [Node.js os module documentation](https://nodejs.org/api/os.html)
- [NPM temp library](https://www.npmjs.com/package/temp)
- [NPM tmp library](https://www.npmjs.com/package/tmp)
