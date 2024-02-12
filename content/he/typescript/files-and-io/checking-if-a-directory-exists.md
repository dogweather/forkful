---
title:                "בדיקה אם ספרייה קיימת"
aliases:
- /he/typescript/checking-if-a-directory-exists/
date:                  2024-02-03T19:09:03.591959-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
