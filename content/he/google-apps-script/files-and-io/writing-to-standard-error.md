---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:48.054628-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Google Apps Script,\
  \ \u05D4\u05D9\u05D0 \u05E9\u05E4\u05EA \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\
  \u05DD \u05DC\u05E4\u05D9\u05EA\u05D5\u05D7 \u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\
  \u05DD \u05E7\u05DC\u05D9\u05DD \u05D1\u05E4\u05DC\u05D8\u05E4\u05D5\u05E8\u05DE\
  \u05EA Google Apps, \u05D0\u05D9\u05E0\u05D4 \u05DE\u05E1\u05E4\u05E7\u05EA \u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05D9\
  \u05E9\u05D9\u05E8\u05D4 \u05DB\u05DE\u05D5 `console.error()` \u05DC\u05DB\u05EA\
  \u05D9\u05D1\u05D4 \u05DC\u2026"
lastmod: '2024-03-13T22:44:38.593617-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, \u05D4\u05D9\u05D0 \u05E9\u05E4\u05EA \u05E1\u05E7\u05E8\
  \u05D9\u05E4\u05D8\u05D9\u05DD \u05DC\u05E4\u05D9\u05EA\u05D5\u05D7 \u05D9\u05D9\
  \u05E9\u05D5\u05DE\u05D9\u05DD \u05E7\u05DC\u05D9\u05DD \u05D1\u05E4\u05DC\u05D8\
  \u05E4\u05D5\u05E8\u05DE\u05EA Google Apps, \u05D0\u05D9\u05E0\u05D4 \u05DE\u05E1\
  \u05E4\u05E7\u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05DE\u05D5\u05D1\
  \u05E0\u05D9\u05EA \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DB\u05DE\u05D5 `console.error()`\
  \ \u05DC\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC stderr, \u05DB\u05E4\u05D9 \u05E9\u05EA\
  \u05D5\u05DB\u05DC \u05DC\u05DE\u05E6\u05D5\u05D0 \u05D1-Node.js \u05D0\u05D5 Python."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

## איך לעשות:
Google Apps Script, היא שפת סקריפטים לפיתוח יישומים קלים בפלטפורמת Google Apps, אינה מספקת פונקציה מובנית ישירה כמו `console.error()` לכתיבה ל stderr, כפי שתוכל למצוא ב-Node.js או Python. עם זאת, ניתן לחקות התנהגות זו על ידי שימוש בשירותי התיעוד של Google Apps Script או טיפול מותאם אישית בשגיאות כדי לנהל ולהפריד פלטי שגיאות.

### דוגמה: שימוש ב-`Logger` להודעות שגיאה
```javascript
function logError() {
  try {
    // סימולציה של שגיאה
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Attempted division by zero");
  } catch (e) {
    // כתבו הודעת שגיאה ליומנים
    Logger.log('Error: ' + e.message);
  }
}
```

כאשר אתה מריץ את `logError()`, זה יכתוב את הודעת השגיאה ליומן של Google Apps Script, אותו ניתן להציג ב-`View > Logs`. זה לא בדיוק stderr, אבל זה משרת מטרה דומה של הפרדת יומני שגיאות מפלטים סטנדרטיים.

### תיעוד אבחון מתקדם
לצורך ניפוי באגים ותיעוד שגיאה מתקדמים יותר, תוכל להשתמש ב-Stackdriver Logging, שכיום נקרא Google Cloud's Operations Suite.

```javascript
function advancedErrorLogging() {
  try {
    // גרימת שגיאה בכוונה
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Error encountered: ', e.toString());
  }
}
```

זה יכוון את הודעת השגיאה ל-Stackdriver Logging, שם היא נוהלת כיומן ברמת שגיאה. שים לב שאינטגרציה של Stackdriver / Google Cloud’s Operations Suite מציעה פתרון תיעוד גרנולרי וניתן לחיפוש יותר לעומת `Logger`.

## חקירה עמוקה
העדר זרם `stderr` מוקדש ב-Google Apps Script משקף את אופיו ומקורותיו כשפת סקריפטים מבוססת ענן, שבה פלטים מבוססי קונסול או טרמינל רגילים (כמו stdout ו-stderr) פחות רלוונטיים. בהיסטוריה, Google Apps Script תוכננה לשיפור פונקציונליות של Google Apps עם סקריפטים פשוטים, תוך דגש על קלות שימוש במקום על תכונות מקיפות זמינות בסביבות תכנות מורכבות יותר.

עם זאת, האבולוציה של Google Apps Script לעבר פיתוח יישומים מתוחכמים יותר הניעה מפתחים לאמץ גישות יצירתיות עבור טיפול בשגיאות ותיעוד, באמצעות שימוש בשירותים זמינים כמו Logger ואינטגרציה עם Google Cloud’s Operations Suite. שיטות אלו, תוך שהן אינן יישומים ישירים של stderr, מציעות חלופות עמידות לניהול שגיאות ותיעוד אבחוני בסביבה ממוקדת ענן.

באופן קריטי, תוך ששיטות אלו משרתות את המטרה בתוך אקוסיסטם של Google Apps Script, הן מדגישות את המגבלות של הפלטפורמה לעומת סביבות תכנות מסורתיות. למפתחים הדורשים אסטרטגיות ניהול שגיאות מפורטות והיררכיות, אינטגרציה עם שירותי תיעוד חיצוניים או אימוץ Google Cloud Functions, שמציעים טיפול יותר מסורתי ב-stderr ו-stdout, עשוי להיות מועדף.
