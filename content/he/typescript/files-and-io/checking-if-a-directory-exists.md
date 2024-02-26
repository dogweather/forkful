---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:03.591959-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D4\u05D0\u05DD \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-TypeScript \u05D4\u05D9\
  \u05D0 \u05D7\u05D9\u05D5\u05E0\u05D9\u05EA \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\
  \u05EA \u05E0\u05D9\u05D4\u05D5\u05DC \u05E7\u05D1\u05E6\u05D9\u05DD, \u05DB\u05D2\
  \u05D5\u05DF \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05E7\u05D1\u05E6\u05D9\u05DD\
  \ \u05D0\u05D5 \u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4\u05DD, \u05D5\
  \u05DE\u05D1\u05D8\u05D9\u05D7\u05D4 \u05E9\u05D4\u05E4\u05E2\u05D5\u05DC\u05D5\u05EA\
  \ \u05DE\u05EA\u05D1\u05E6\u05E2\u05D5\u05EA \u05E8\u05E7 \u05E2\u05DC \u05E1\u05E4\
  \u05E8\u05D9\u05D5\u05EA \u05D7\u05D5\u05E7\u05D9\u05D5\u05EA. \u05E4\u05E2\u05D5\
  \u05DC\u05D4\u2026"
lastmod: '2024-02-25T18:49:37.180792-07:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D4\u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-TypeScript \u05D4\u05D9\u05D0\
  \ \u05D7\u05D9\u05D5\u05E0\u05D9\u05EA \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA\
  \ \u05E0\u05D9\u05D4\u05D5\u05DC \u05E7\u05D1\u05E6\u05D9\u05DD, \u05DB\u05D2\u05D5\
  \u05DF \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05E7\u05D1\u05E6\u05D9\u05DD \u05D0\
  \u05D5 \u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4\u05DD, \u05D5\u05DE\
  \u05D1\u05D8\u05D9\u05D7\u05D4 \u05E9\u05D4\u05E4\u05E2\u05D5\u05DC\u05D5\u05EA\
  \ \u05DE\u05EA\u05D1\u05E6\u05E2\u05D5\u05EA \u05E8\u05E7 \u05E2\u05DC \u05E1\u05E4\
  \u05E8\u05D9\u05D5\u05EA \u05D7\u05D5\u05E7\u05D9\u05D5\u05EA. \u05E4\u05E2\u05D5\
  \u05DC\u05D4\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה האם ספרייה קיימת ב-TypeScript היא חיונית למשימות ניהול קבצים, כגון קריאה מקבצים או כתיבה אליהם, ומבטיחה שהפעולות מתבצעות רק על ספריות חוקיות. פעולה זו קריטית כדי למנוע שגיאות הנובעות מניסיונות לגשת לספריות שאינן קיימות או לשנות אותן.

## איך לעשות:

TypeScript, כשהוא רץ בסביבת Node.js, מאפשר לבדוק אם ספרייה קיימת באמצעות המודול `fs`, המספק את הפונקציה `existsSync()` או את הפונקציה הא-סינכרונית `access()` בשילוב עם `constants.F_OK`.

### שימוש ב-`fs.existsSync()`:

```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('הספרייה קיימת.');
} else {
  console.log('הספרייה אינה קיימת.');
}
```

### שימוש ב-`fs.access()` עם `fs.constants.F_OK`:

```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('הספרייה אינה קיימת.');
    return;
  }
  console.log('הספרייה קיימת.');
});
```

**דוגמה לפלט** עבור שני השיטות, בהנחה שהספרייה קיימת:
```
הספרייה קיימת.
```

ואם היא אינה קיימת:
```
הספרייה אינה קיימת.
```

### שימוש בספרייה חיצונית - `fs-extra`:

`fs-extra` היא ספרייה חיצונית פופולרית שמעשירה את המודול `fs` הקיים ומספקת פונקציות נוחות יותר.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`הספרייה קיימת: ${exists}`);
});
```

**דוגמה לפלט** כאשר הספרייה קיימת:
```
הספרייה קיימת: true
```

ואם היא אינה קיימת:
```
הספרייה קיימת: false
```
