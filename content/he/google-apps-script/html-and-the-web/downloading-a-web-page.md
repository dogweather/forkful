---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:08.447628-07:00
description: "\u05D4\u05D5\u05E8\u05D3\u05D4 \u05E9\u05DC \u05D3\u05E3 \u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05E0\u05D8 \u05D1-Google Apps Script \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05D0\u05EA \u05EA\u05E4\u05D9\u05E1\u05EA \u05EA\u05D5\u05DB\u05DF \u05E9\
  \u05DC \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D3\u05E8\u05DA\
  \ HTML \u05DC\u05DE\u05D8\u05E8\u05D5\u05EA \u05E9\u05D5\u05E0\u05D5\u05EA, \u05DB\
  \u05DE\u05D5 \u05DC\u05DE\u05E9\u05DC \u05D2\u05E8\u05D9\u05E4\u05EA \u05E8\u05E9\
  \u05EA, \u05D7\u05D9\u05DC\u05D5\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\
  \u05D5 \u05DE\u05E2\u05E7\u05D1 \u05D0\u05D7\u05E8 \u05E9\u05D9\u05E0\u05D5\u05D9\
  \u05D9\u05DD.\u2026"
lastmod: '2024-03-13T22:44:38.562848-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05D5\u05E8\u05D3\u05D4 \u05E9\u05DC \u05D3\u05E3 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8 \u05D1-Google Apps Script \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05D0\u05EA \u05EA\u05E4\u05D9\u05E1\u05EA \u05EA\u05D5\u05DB\u05DF \u05E9\u05DC\
  \ \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D3\u05E8\u05DA HTML\
  \ \u05DC\u05DE\u05D8\u05E8\u05D5\u05EA \u05E9\u05D5\u05E0\u05D5\u05EA, \u05DB\u05DE\
  \u05D5 \u05DC\u05DE\u05E9\u05DC \u05D2\u05E8\u05D9\u05E4\u05EA \u05E8\u05E9\u05EA\
  , \u05D7\u05D9\u05DC\u05D5\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5\
  \ \u05DE\u05E2\u05E7\u05D1 \u05D0\u05D7\u05E8 \u05E9\u05D9\u05E0\u05D5\u05D9\u05D9\
  \u05DD."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05E2\u05DE\u05D5\u05D3 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8"
weight: 42
---

## מה ולמה?

הורדה של דף אינטרנט ב-Google Apps Script כוללת את תפיסת תוכן של דף אינטרנט דרך HTML למטרות שונות, כמו למשל גריפת רשת, חילוץ נתונים, או מעקב אחר שינויים. מתכנתים בוחרים בפעולה זו כדי לאוטמט פעולות של איסוף נתונים או אינטגרציה, במיזם להקטין מאמץ ידני ולהבטיח עיבוד נתונים בזמן אמת.

## איך ל:

ב-Google Apps Script, השירות `UrlFetchApp` הוא קרדיטלי להורדת תוכן אינטרנטי. להלן מדריך צעד אחר צעד ודוגמה פשוטה המדגימה איך לאחזר ולרשום את תוכן HTML של דף אינטרנטי:

1. **פעולת Fetch בסיסית:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- קוד זה אוחזר את תוכן ה-HTML של example.com ומוסיף לו תיעוד. זו הדגמה ישירה של קבלת מקור של דף אינטרנט ללא פרמטרים נוספים.

2. **טיפול בהפניות וב-HTTPS:**

עבור HTTPS או טיפול בהפניות, הקוד נשאר בעיקרו זהה, אך שקול ליישם טיפול בשגיאות או אפשרויות ספציפיות להפניות:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // באופן אוטומטי עוקב אחרי הפניות
    'muteHttpExceptions': true // השתקת חריגות אפשריות לטיפול חלק בהן
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **מגבלות קצב ומכסות:**

הייה מודע למגבלות ולמכסות של Google Apps Script; שימוש כבד עלול לדרוש טיפול בשגיאות הנובעות ממגבלות קצב.

## חקירה עמוקה

היסטורית, הורדת וניהול תוכן אינטרנטי התחילו עם בקשות HTTP פשוטות, שהתפתחו באופן משמעותי עם צמיחת שפות תיכנות. Google Apps Script מאפשרת ביצוע פשוט של משימות כאלה תוך ניצול תשתית חזקה של גוגל באקוסיסטם של G Suite. השירות `UrlFetchApp` הוא רכיב גרעיני של פונקציונליות זו, המכיל בקשות HTTP/S מורכבות בממשק יישום פשוט יותר.

למרות הנוחות שלו, Google Apps Script יכול להיות לא תמיד הכלי הטוב ביותר עבור גריפת אינטרנט בעוצמה גבוהה או כאשר נדרשת עיבוד אחרי לכידה מורכבת של נתונים עקב מגבלות זמן הביצוע והמכסות שגוגל מטילה. במקרים כאלו, מערכות או שפות תיכנות מיועדות לגריפת אינטרנט או שעוצבו לפעולות קלט/פלט אסינכרוניות, כמו Node.js עם ספריות כמו Puppeteer או Cheerio, עשויות להציע גמישות ועוצמה רבה יותר.

בנוסף, כשGoogle Apps Script היא כלי מעולה לאינטגרציה עם שירותי Google (כמו Sheets, Docs, ו-Drive) וביצוע פעולות איסוף נתונים קלות, חשוב לשים לב למגבלות של סביבת הביצוע שלה. עבור משימות מרובות, שקול להשתמש ב-Google Cloud Functions או בשירותים מתקדמים של Apps Script עם משאבי חישוב חיצוניים לעיבוד.
