---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:19.276282-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05D1-Node.js, \u05E0\u05D9\u05EA\u05DF \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\
  -stderr \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E9\u05D9\u05D8\u05D4\
  \ `console.error()` \u05D0\u05D5 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05DB\u05EA\u05D9\
  \u05D1\u05D4 \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC-`process.stderr`. \u05D4\u05E0\
  \u05D4 \u05D3\u05D5\u05D2\u05DE\u05D0\u05D5\u05EA \u05D4\u05DE\u05D3\u05D2\u05D9\
  \u05DE\u05D5\u05EA \u05D0\u05EA \u05E9\u05EA\u05D9 \u05D4\u05D2\u05D9\u05E9\u05D5\
  \u05EA."
lastmod: '2024-03-13T22:44:40.002352-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Node.js, \u05E0\u05D9\u05EA\u05DF \u05DC\u05DB\u05EA\u05D5\u05D1\
  \ \u05DC-stderr \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E9\u05D9\u05D8\
  \u05D4 `console.error()` \u05D0\u05D5 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05DB\u05EA\
  \u05D9\u05D1\u05D4 \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC-`process.stderr`."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

## איך לעשות זאת:
ב-Node.js, ניתן לכתוב ל-stderr באמצעות השיטה `console.error()` או על ידי כתיבה ישירה ל-`process.stderr`. הנה דוגמאות המדגימות את שתי הגישות:

```javascript
// באמצעות console.error()
console.error('זוהי הודעת שגיאה.');

// כתיבה ישירה ל-process.stderr
process.stderr.write('זוהי הודעת שגיאה נוספת.\n');
```

פלט לדוגמה עבור שתי השיטות יופיע בזרם ה-stderr, ללא ערבוב עם stdout:
```
זוהי הודעת שגיאה.
זוהי הודעת שגיאה נוספת.
```

לצורכי תיעוד יותר מתוחכם או ספציפי ליישום, מפתחי JavaScript רבים משתמשים בספריות צד שלישי כמו `winston` או `bunyan`. הנה דוגמה מהירה באמצעות `winston`:

תחילה, התקן את `winston` דרך npm:
```shell
npm install winston
```

לאחר מכן, קבע את `winston` לתעד שגיאות ל-stderr:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// תיעוד הודעת שגיאה
logger.error('שגיאה תועדה דרך winston.');
```

הגדרה זו מבטיחה שכאשר אתה מתעד שגיאה באמצעות `winston`, היא מופנית ל-stderr, מה שעוזר לשמור על הפרדה ברורה בין פלט סטנדרטי לבין פלט שגיאות.
