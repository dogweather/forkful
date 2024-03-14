---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:03.769785-07:00
description: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\
  \u05DC \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05D8\u05E7\u05E1\u05D8 \u05E9\u05DE\
  \u05D9\u05D9\u05E6\u05D2 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05D0\u05D5\u05D1\
  \u05D9\u05D9\u05E7\u05D8 \u05EA\u05D0\u05E8\u05D9\u05DA, \u05DE\u05D4 \u05E9\u05DE\
  \u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\
  \u05D1\u05E6\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05D4\u05E7\u05E9\u05D5\
  \u05E8\u05D5\u05EA \u05DC\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DB\u05DE\
  \u05D5 \u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA, \u05D7\u05D9\u05E9\u05D5\u05D1\
  \u05D9\u05DD \u05D5\u05E2\u05D9\u05E6\u05D5\u05D1. \u05D6\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.581632-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC\
  \ \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05D8\u05E7\u05E1\u05D8 \u05E9\u05DE\u05D9\
  \u05D9\u05E6\u05D2 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05D0\u05D5\u05D1\u05D9\
  \u05D9\u05E7\u05D8 \u05EA\u05D0\u05E8\u05D9\u05DA, \u05DE\u05D4 \u05E9\u05DE\u05D0\
  \u05E4\u05E9\u05E8 \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05D1\
  \u05E6\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05D4\u05E7\u05E9\u05D5\u05E8\
  \u05D5\u05EA \u05DC\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DB\u05DE\u05D5\
  \ \u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA, \u05D7\u05D9\u05E9\u05D5\u05D1\u05D9\
  \u05DD \u05D5\u05E2\u05D9\u05E6\u05D5\u05D1. \u05D6\u05D4\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

פיענוח תאריך מתוך מחרוזת כולל המרה של טקסט שמייצג תאריך לאובייקט תאריך, מה שמאפשר למתכנתים לבצע פעולות הקשורות לתאריכים כמו השוואות, חישובים ועיצוב. זה חיוני לטיפול בקלט מהמשתמש, עיבוד נתונים ממקורות חיצוניים וניהול תאריכים בפורמטים שונים, במיוחד באפליקציות שכוללות תכנון, ניתוח נתונים או כל סוג של רישומים המבוססים על זמן.

## איך לעשות:

ב-Google Apps Script, המבוסס על JavaScript, יש לך מספר גישות לפיענוח תאריך מתוך מחרוזת. להלן דוגמאות באמצעות שיטות מקוריות של JavaScript וכלים של Google Apps Script.

**שימוש ב-`new Date()` constructor:**

הדרך הפשוטה ביותר לפרש מחרוזת לתאריך ב-Google Apps Script היא באמצעות בנאי האובייקט `Date`. עם זאת, זה דורש מהמחרוזת להיות בפורמט המזוהה על ידי המתודה Date.parse()‎ (למשל, YYYY-MM-DD).

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // רושם Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)
```

**שימוש ב-`Utilities.parseDate()`:**

לגמישות רבה יותר, במיוחד עם פורמטים מותאמים אישית של תאריכים, Google Apps Script מספקת את `Utilities.parseDate()`. שיטה זו מאפשרת לך לציין את פורמט התאריך, אזור הזמן והלוקאל.

```javascript
const dateString = '01-04-2023'; // DD-MM-YYYY
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // רושם Sat Apr 01 2023 00:00:00 GMT+0000 (UTC) בהתאם לאזור הזמן של הסקריפט
```

שימו לב: אף ש-`Utilities.parseDate()` מציע יותר שליטה, התנהגותו יכולה להשתנות בהתבסס על אזור הזמן של הסקריפט, ולכן קריטי לציין במפורש את אזור הזמן אם האפליקציה שלך מטפלת בתאריכים ממספר אזורים.

## חקירה עמוקה

פריסת תאריכים בשפות תכנות היסטורית הייתה מלווה באתגרים, בעיקר בשל מגוון פורמטים של תאריכים ומורכבויות של אזורי זמן. גישת Google Apps Script, הנגזרת בעיקר מ-JavaScript, שואפת לפשט זאת על ידי הצעת האובייקט `Date` הפשוט וגם את הפונקציה המגוונת יותר `Utilities.parseDate()`. עם זאת, לכל שיטה יש הגבלות; לדוגמה, הסתמכות על בנאי ה-`Date` עם מחרוזות יכולה להוביל לחוסר עקביות בסביבות שונות בשל פרשנויות שונות של פורמטים של תאריכים. מצד שני, `Utilities.parseDate()` דורש הבנה ברורה יותר של הפורמט, אזור הזמן והלוקאל, דבר שהופך אותו למורכב יותר אך יותר אמין לצרכים מסוימים.

ספריות או שירותים חלופיים, כמו Moment.js (כעת ממליצים על Luxon לפרויקטים חדשים), מספקים פונקציונליות עשירה יותר וטיפול טוב יותר באזורי זמן, תוך פתרון רבים מהאתגרים הללו. עם זאת, בהקשר של Google Apps Script, שם יש הגבלות על ספריות חיצוניות, הבנה ושימוש יעיל בשיטות הפנימיות הופכים לקריטיים. מתכנתים הבאים משפות אחרות יכולים למצוא את העדינויות של טיפול בתאריכים ב-Google Apps Script כאתגר ייחודי אך יכולים להשיג פיענוח תאריכים עמיד עם הבנה עמוקה של הכלים הזמינים ושיקול זהיר של האופי הגלובלי של האפליקציות שלהם.
