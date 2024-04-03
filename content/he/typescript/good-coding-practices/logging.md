---
date: 2024-01-26 01:09:58.541391-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-TypeScript,\
  \ \u05E0\u05D9\u05EA\u05DF \u05DC\u05D9\u05D9\u05E9\u05DD \u05D1\u05E7\u05DC\u05D5\
  \u05EA \u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\u05D9\u05D8\u05D5\u05EA \u05E9\u05DC\
  \ console \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D1 \u05DC\u05D5\u05D2\u05D9\u05E0\u05D2\
  \ \u05DE\u05EA\u05E7\u05D3\u05DD \u05D9\u05D5\u05EA\u05E8 \u05E2\u05DD \u05E1\u05E4\
  \u05E8\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5 `winston` \u05D0\u05D5 `pino`. \u05DC\
  \u05D4\u05DC\u05DF \u05D3\u05D5\u05D2\u05DE\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.928170-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-TypeScript, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D9\u05D9\u05E9\u05DD\
  \ \u05D1\u05E7\u05DC\u05D5\u05EA \u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 \u05D1\u05E1\
  \u05D9\u05E1\u05D9 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\u05D9\u05D8\
  \u05D5\u05EA \u05E9\u05DC console \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D1 \u05DC\u05D5\
  \u05D2\u05D9\u05E0\u05D2 \u05DE\u05EA\u05E7\u05D3\u05DD \u05D9\u05D5\u05EA\u05E8\
  \ \u05E2\u05DD \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5 `winston`\
  \ \u05D0\u05D5 `pino`."
title: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA (\u05DC\
  \u05D5\u05D2\u05D9\u05DD)"
weight: 17
---

## איך לעשות:
ב-TypeScript, ניתן ליישם בקלות לוגינג בסיסי באמצעות שיטות של console או לשלב לוגינג מתקדם יותר עם ספריות כמו `winston` או `pino`. להלן דוגמה בסיסית באמצעות `console.log` ודוגמה מתקדמת יותר עם `winston`.

```TypeScript
// יומנות בסיסית של console
console.log('Info: Starting the application...');
console.error('Error: Unable to retrieve data.');

// פלט לדוגמה
// Info: Starting the application...
// Error: Unable to retrieve data.
```

ללוגינג יותר עמיד, בואו נקים את `winston`:

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('Server started!');
logger.warn('Low disk space warning.');
logger.error('Failed to connect to database.');

// פלט לדוגמה ב-combined.log
// 2023-01-20 14:42:07 info: Server started!
// 2023-01-20 14:42:09 warn: Low disk space warning.
// 2023-01-20 14:42:12 error: Failed to connect to database.
```

## עיון נוסף:
המושג לוגינג בהקשר של מחשוב מתייחס לימים הראשונים של תכנות, שם המונח בעצמו נגזר מ"יומן הסיפון", מערכת תיעוד לשימור רשומות של ספינות. בדרך כלל, אירועים בתוכנית רושמו להדפסות פיזיות או לפלטים של טרמינל, במיוחד בתקופת עידן המיינפריים.

קדימה להיום, ויש לך כמות עצומה של כלים וספריות לרשותך שמספקים פתרונות לכל צורך לוגינג, החל מקבצי טקסט פשוטים ועד מערכות ניהול לוגים מורכבות. חלופות ל-`winston` כוללות את `pino`, הנהדרת בביצועים גבוהים, ואת `Bunyan`, המבוססת על JSON. כאשר עובדים עם Node.js, ספריות לוגינג לעיתים קרובות מספקות מנגנוני זרם להכוון יומנים ליעדים שונים, תמיכה ברוטציה של היומנים, ועיצובים הניתנים להתאמה אישית.

מבחינה טכנית, הודעת לוג בדרך כלל מכילה חותמת זמן, רמת חומרה (כמו info, warn, error), וההודעה עצמה. מנהג טוב בלוגינג ממליץ לקטלג נכון את רמות הלוג, להימנע מנתונים רגישים ביומנים, ולשקול השלכות ביצועים ביישומים עם גודל רב של תעבורה.

## ראה גם:
- [Winston - מקליט לכמעט הכל](https://www.npmjs.com/package/winston)
- [Pino - מקליט Node.js עם עומס נמוך מאוד](https://www.npmjs.com/package/pino)
- [מנהגים מומלצים ללוגינג ב-Node.js](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [האפליקציה ב-12 פקטורים - לוגים](https://12factor.net/logs)
