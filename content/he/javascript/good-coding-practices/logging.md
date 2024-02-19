---
aliases:
- /he/javascript/logging/
date: 2024-01-26 01:08:57.572387-07:00
description: "\u05DC\u05D5\u05D2\u05D9\u05E0\u05D2, \u05D1\u05DE\u05D9\u05DC\u05D9\
  \u05DD \u05E4\u05E9\u05D5\u05D8\u05D5\u05EA, \u05D6\u05D4 \u05DB\u05DE\u05D5 \u05DC\
  \u05E9\u05DE\u05D5\u05E8 \u05D9\u05D5\u05DE\u05DF \u05DC\u05D0\u05E4\u05DC\u05D9\
  \u05E7\u05E6\u05D9\u05D4 \u05E9\u05DC\u05DA \u2013 \u05D4\u05D5\u05D0 \u05E8\u05D5\
  \u05E9\u05DD \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05E9\u05D2\u05D9\u05D0\
  \u05D5\u05EA, \u05D5\u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05DE\u05E9\u05DE\u05E2\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA \u05E9\u05E7\u05D5\
  \u05E8\u05D5\u05EA \u05D1\u05D6\u05DE\u05DF \u05E9\u05D4\u05EA\u05D5\u05DB\u05E0\
  \u05D4 \u05E8\u05E6\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05D0\u2026"
lastmod: 2024-02-18 23:08:53.255080
model: gpt-4-1106-preview
summary: "\u05DC\u05D5\u05D2\u05D9\u05E0\u05D2, \u05D1\u05DE\u05D9\u05DC\u05D9\u05DD\
  \ \u05E4\u05E9\u05D5\u05D8\u05D5\u05EA, \u05D6\u05D4 \u05DB\u05DE\u05D5 \u05DC\u05E9\
  \u05DE\u05D5\u05E8 \u05D9\u05D5\u05DE\u05DF \u05DC\u05D0\u05E4\u05DC\u05D9\u05E7\
  \u05E6\u05D9\u05D4 \u05E9\u05DC\u05DA \u2013 \u05D4\u05D5\u05D0 \u05E8\u05D5\u05E9\
  \u05DD \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05E9\u05D2\u05D9\u05D0\u05D5\
  \u05EA, \u05D5\u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05DE\u05E9\u05DE\u05E2\u05D5\
  \u05EA\u05D9\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA \u05E9\u05E7\u05D5\u05E8\
  \u05D5\u05EA \u05D1\u05D6\u05DE\u05DF \u05E9\u05D4\u05EA\u05D5\u05DB\u05E0\u05D4\
  \ \u05E8\u05E6\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05D0\u2026"
title: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA (\u05DC\
  \u05D5\u05D2\u05D9\u05DD)"
---

{{< edit_this_page >}}

## מה ולמה?
לוגינג, במילים פשוטות, זה כמו לשמור יומן לאפליקציה שלך – הוא רושם אירועים, שגיאות, ופעולות משמעותיות אחרות שקורות בזמן שהתוכנה רצה. מתכנתים עושים זאת לא רק כדי להבין מה קורה מתחת למכסה בזמן אמת, אלא גם כדי שיהיה להם רשומה היסטורית שהיא קריטית לצורך דיבאגינג, בדיקות ואופטימיזציה של ביצועים.

## איך לעשות:
מבלי להתאמץ, ג'אווהסקריפט מציעה דרך פשוטה לרשום הודעות בקונסול:

```javascript
console.log('זה ירושם לקונסול');

// פלט:
// זה ירושם לקונסול
```

אבל אפליקציות בעולם האמיתי דורשות יותר מכך מאשר רק הדפסת הודעות לקונסול. ניתן להכניס ספריות כמו Winston או Pino על מנת לנהל לוגים בצורה יעילה:

```javascript
// שימוש ב-Winston ללוגינג מתקדם
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('שלום, זו אירוע לוגינג עם Winston');
// הלוג הזה נכתב ל-'combined.log' בפורמט JSON
```

דוגמא לפלט של `combined.log`:

```json
{"message":"שלום, זו אירוע לוגינג עם Winston","level":"info"}
```

## צלילה עמוקה
לוגינג היה חיוני מהימים הראשונים של המחשוב; מפעילי מערכת היו סוקרים לוגים כדי להבין את ביצועי המערכת ולאבחן בעיות. קדימה לפיתוח מודרני, ואנחנו עברנו מקבצי לוג פשוטים למערכות ניהול לוג מבניות וניתנות לחיפוש.

חלופות ללוגינג בקונסול או בהתבסס על קבצים בג'אווהסקריפט כוללות שימוש בשירותי לוגינג מבוססי ענן כמו Loggly, Datadog, או ELK Stack (Elasticsearch, Logstash, Kibana) אשר יכולים לאגד לוגים ממקורות מרובים, להציע כלים לוויזואליזציה ואנליטיקה מתקדמת.

כאשר מיישמים לוגינג, יש לשקול את הדברים הבאים:
- **רמת הפירוט**: כולל debug, info, warning, error, ו-critical.
- **ביצועים**: לוגינג מוגזם יכול להשפיע על ביצועי האפליקציה.
- **אבטחה**: היזהר ברישום מידע רגיש.
- **פורמט**: לוגים מבניים (כמו JSON) הופכים את חיפוש הלוגים וניתוחם לקל יותר.
- **מדיניות שמירה**: יש לארכב או למחוק לוגים ישנים כדי לחסוך במקום.

אסטרטגיית לוגינג מעשית מגדירה מה לרשום, איפה לרשום, וכמה זמן לשמור את זה, תוך שילוב תובנה מועילה לעומת שיקולי ביצועים ופרטיות.

## ראה גם
בדוק את המשאבים הבאים לצלילה עמוקה יותר:
- [Winston GitHub Repository](https://github.com/winstonjs/winston): לשימוש מעמיק ו-transport מותאמים אישית.
- [Pino - Node.js logger בעל overhead נמוך מאוד](https://github.com/pinojs/pino): פתרון לוגינג קל משקל.
- [MDN Web Docs: Console](https://developer.mozilla.org/en-US/docs/Web/API/Console): למידע בסיס ללוגינג מבוסס דפדפן.
- [Elastic ELK Stack](https://www.elastic.co/what-is/elk-stack): טריו עוצמתי לניהול לוגים.
- [12 Factor App Logging](https://12factor.net/logs): מיטב המתודולוגיות בלוגינג של אפליקציות.
