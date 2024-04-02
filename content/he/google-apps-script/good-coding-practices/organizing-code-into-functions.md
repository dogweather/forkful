---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:19.085136-07:00
description: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\
  \u05DA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05D6\u05D4\u05D5 \u05E2\
  \u05E0\u05D9\u05D9\u05DF \u05E9\u05DC \u05DE\u05D1\u05E0\u05D4 \u05E9\u05DC \u05E7\
  \u05D5\u05D3 Google Apps Script \u05E9\u05DC\u05DA \u05E2\u05DC \u05D9\u05D3\u05D9\
  \ \u05D4\u05E4\u05E8\u05D3\u05EA \u05E7\u05D8\u05E2\u05D9\u05DD \u05DC\u05D5\u05D2\
  \u05D9\u05D9\u05DD \u05DC\u05D1\u05DC\u05D5\u05E7\u05D9\u05DD \u05D1\u05E8\u05D5\
  \u05E8\u05D9\u05DD, \u05DB\u05DC \u05D0\u05D7\u05D3 \u05DE\u05D1\u05E6\u05E2 \u05DE\
  \u05E9\u05D9\u05DE\u05D4 \u05DE\u05E1\u05D5\u05D9\u05D9\u05DE\u05EA. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.574780-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05D6\u05D4\u05D5 \u05E2\u05E0\
  \u05D9\u05D9\u05DF \u05E9\u05DC \u05DE\u05D1\u05E0\u05D4 \u05E9\u05DC \u05E7\u05D5\
  \u05D3 Google Apps Script \u05E9\u05DC\u05DA \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\
  \u05E4\u05E8\u05D3\u05EA \u05E7\u05D8\u05E2\u05D9\u05DD \u05DC\u05D5\u05D2\u05D9\
  \u05D9\u05DD \u05DC\u05D1\u05DC\u05D5\u05E7\u05D9\u05DD \u05D1\u05E8\u05D5\u05E8\
  \u05D9\u05DD, \u05DB\u05DC \u05D0\u05D7\u05D3 \u05DE\u05D1\u05E6\u05E2 \u05DE\u05E9\
  \u05D9\u05DE\u05D4 \u05DE\u05E1\u05D5\u05D9\u05D9\u05DE\u05EA. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\u2026"
title: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
weight: 18
---

## מה ולמה?

ארגון קוד לתוך פונקציות זהו עניין של מבנה של קוד Google Apps Script שלך על ידי הפרדת קטעים לוגיים לבלוקים ברורים, כל אחד מבצע משימה מסויימת. מתכנתים עושים זאת על מנת לשפר את נגישות הקוד, ניתן לתחזוקה וניתן לשימוש מחדש, באופן שיבטיח שתסריטים מורכבים יהיו קלים יותר להבנה ולניפוי באגים.

## איך לעשות:

ב-Google Apps Script, המבוסס על JavaScript, מגדירים פונקציות באמצעות מילת המפתח `function`, בעקבותיה שם הפונקציה הייחודי, סוגריים `()` שיכולים להכיל פרמטרים, וסוגריים מסולסלים `{}` המכלילים את בלוק הקוד של הפונקציה. הנה דוגמא בסיסית:

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('שלום, ' + user + '!');
}

greetUser();
```

פלט לדוגמא:

```
שלום, someone@example.com!
```

עכשיו, בואו נבחן דוגמא יותר מעשית הקשורה ל-Google Sheets שבה אנו מפרידים את הפונקציונליות לשתי פונקציות: אחת להכנת הגיליון ואחרת למילוי זה בנתונים.

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('נתוני מכירות');
  sheet.appendRow(['פריט', 'כמות', 'מחיר']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('נתוני מכירות');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// הכנת מערך של נתונים
var salesData = [
  ['Widgets', 15, 2.5],
  ['Gadgets', 8, 3.75]
];

// הפעלת הפונקציות
setupSheet();
populateSheet(salesData);
```

בדוגמא זו, `setupSheet` מכינה את הגיליון, ו-`populateSheet` לוקח מערך של נתוני מכירות כדי למלא את הגיליון. הפרדת דאגות אלו הופכת את הקוד לנקי יותר ויותר נתן להתאמה לשינויים.

## צלילה עמוקה

המושג של חלוקת קוד לפונקציות אינו חדש או ייחודי ל-Google Apps Script; זוהי תרגולת תכנות יסודית המומלצת בכמעט כל שפות התכנות. בהיסטורייה, פונקציות התפתחו מהמושג המתמטי של מיפוי קלטים לפלטים, שהפך לאבן פינה בתכנות מובנה. גישה זו מקדמת מודולריות ושימוש חוזר בקוד, מציעה מסלולים ברורים לבדיקה של חלקים בודדים של התסריט.

Google Apps Script, המבוסס על JavaScript, נהנה במיוחד מפונקציות מעלה ראשונה של JavaScript, שמאפשרות לפונקציות להימסר כארגומנטים, להיחזר מפונקציות אחרות, ולהיות מוקצות למשתנים. תכונה זו פותחת דפוסים מתקדמים כמו קולבקים ותכנות פונקציונלי, אך דפוסים אלו יכולים להוסיף מורכבות שעשויה להיות מיותרת למשימות אוטומציה פשוטות ב-Google Apps Script.

לפרויקטים גדולים יותר או יישומים מורכבים יותר, מפתחים עשויים לחקור את שימוש בתכונות חדשות יותר של JavaScript כמו פונקציות חץ, async/await לפעולות אסינכרוניות, ואף TypeScript להקלדה סטטית. TypeScript, במיוחד, יכול להיקלט לריצה כ-Google Apps Script, מספק מסלול למפתחים המחפשים בדיקה רובוסטית יותר של סוגים ותכונות מתקדמות מונחות עצמים.

עם זאת, לרוב צרכי התסריטים בחבילת Google Apps, היצמדות לפונקציות פשוטות ומאורגנות היטב כפי שהוצג מספקת בסיס חזק. זה תמיד אקט של איזון בין שימוש בתכונות מתקדמות ליעילות ושמירה על פשטות לקלות תחזוקה ונגישות.
