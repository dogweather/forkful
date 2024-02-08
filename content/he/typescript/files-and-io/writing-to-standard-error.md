---
title:                "כתיבה לשגיאה התקנית"
date:                  2024-02-03T19:35:08.240892-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
ב-TypeScript, כתיבה לשגיאה סטנדרטית (stderr) היא תהליך של שליחת הודעות שגיאה או יומני רישום ישירות לזרם הפלט של השגיאה של הסביבה (למשל, הקונסול ב-node.js או בדפדפן אינטרנט). זה חיוני לאבחון בעיות מבלי להתערב בפלט הסטנדרטי (stdout) המשמש בדרך כלל לנתוני תוכנית, ומבטיח שטיפול בשגיאות ותיעוד מתבצעים באופן יעיל ומקושר.

## איך לעשות זאת:
TypeScript, בהיותו על-קבוצה של JavaScript, מסתמך על סביבת הזמן הרץ הבסיסית של JS (כמו Node.js) לכתיבה ל-stderr. הנה איך אפשר לעשות זאת באופן ישיר:

```typescript
console.error("זהו הודעת שגיאה.");
```

דוגמה לפלט ל-stderr:
```
זהו הודעת שגיאה.
```

בסביבת Node.js, ניתן גם להשתמש בשיטת `process.stderr.write()` לכתיבה ברמה נמוכה יותר:

```typescript
process.stderr.write("הודעת שגיאה ברמה נמוכה.\n");
```

דוגמה לפלט ל-stderr:
```
הודעת שגיאה ברמה נמוכה.
```

עבור תיעוד של שגיאות מובנה יותר, ייתכן שתשתמש בספריות צד שלישי פופולריות כמו `winston` או `pino`. הנה איך לרשום שגיאות באמצעות `winston`:

ראשית, התקן את `winston`:

```bash
npm install winston
```

לאחר מכן, השתמש בו בקובץ ה-TypeScript שלך:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('שגיאה נרשמה באמצעות winston.');
```

זה יכתוב את השגיאה גם לקונסול וגם לקובץ בשם `error.log`. זכור, כאשר כותבים לקבצים, חשוב לנהל הרשאות קובץ ומעבר לקובץ חדש כדי למנוע בעיות הקשורות לשימוש בשטח הדיסק.
