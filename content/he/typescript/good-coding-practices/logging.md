---
title:                "רישום פעולות (לוגים)"
aliases:
- /he/typescript/logging.md
date:                  2024-01-26T01:09:58.541391-07:00
model:                 gpt-4-1106-preview
simple_title:         "רישום פעולות (לוגים)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/logging.md"
---

{{< edit_this_page >}}

## מה ולמה?

לוגינג הוא התהליך של תיעוד אירועים, שגיאות ומידע נוסף חשוב במהלך ביצוע התוכנית למדיום חיצוני, לעיתים קובצים או מסדי נתונים. תוכניתנים משתמשים ביומנים כדי לפקח על התנהגות התוכנה, לנפות בעיות ולעקוב אחר פעילויות מערכת לצורך ניתוח ביטחון וביצועים.

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
