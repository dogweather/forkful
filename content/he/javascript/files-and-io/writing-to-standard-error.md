---
title:                "כתיבה לשגיאה התקנית"
aliases: - /he/javascript/writing-to-standard-error.md
date:                  2024-02-03T19:34:19.276282-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאה סטנדרטית (stderr) ב-JavaScript מדוברת על הכוונת הודעות שגיאה או מידע קריטי לזרם נפרד מסוים, מה שמאוד שימושי בסביבות דמויות-Unix לצורכי תיעוד וניפוי שגיאות. מתכנתים עושים זאת כדי להבחין בין פלט תוכנית רגיל לבין הודעות שגיאה, מה שמאפשר ניהול פלט נקי יותר וניטור שגיאות קל יותר.

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
