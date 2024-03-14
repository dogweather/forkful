---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:08.240892-07:00
description: "\u05D1-TypeScript, \u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\
  \u05D9\u05D0\u05D4 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05D4\
  \u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05DC\u05D9\u05D7\
  \u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D0\
  \u05D5 \u05D9\u05D5\u05DE\u05E0\u05D9 \u05E8\u05D9\u05E9\u05D5\u05DD \u05D9\u05E9\
  \u05D9\u05E8\u05D5\u05EA \u05DC\u05D6\u05E8\u05DD \u05D4\u05E4\u05DC\u05D8 \u05E9\
  \u05DC \u05D4\u05E9\u05D2\u05D9\u05D0\u05D4 \u05E9\u05DC \u05D4\u05E1\u05D1\u05D9\
  \u05D1\u05D4 (\u05DC\u05DE\u05E9\u05DC, \u05D4\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC\
  \ \u05D1-node.js\u2026"
lastmod: '2024-03-13T22:44:38.944907-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-TypeScript, \u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\
  \u05D0\u05D4 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05D4\u05D9\
  \u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05DC\u05D9\u05D7\u05EA\
  \ \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D0\u05D5\
  \ \u05D9\u05D5\u05DE\u05E0\u05D9 \u05E8\u05D9\u05E9\u05D5\u05DD \u05D9\u05E9\u05D9\
  \u05E8\u05D5\u05EA \u05DC\u05D6\u05E8\u05DD \u05D4\u05E4\u05DC\u05D8 \u05E9\u05DC\
  \ \u05D4\u05E9\u05D2\u05D9\u05D0\u05D4 \u05E9\u05DC \u05D4\u05E1\u05D1\u05D9\u05D1\
  \u05D4 (\u05DC\u05DE\u05E9\u05DC, \u05D4\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC \u05D1\
  -node.js\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
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
