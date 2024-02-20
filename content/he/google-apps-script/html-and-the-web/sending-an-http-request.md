---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:14.844798-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D1\
  -Google Apps Script \u05D4\u05D9\u05D0 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D1\
  \u05E6\u05E2 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \u05D9\u05EA \u05DC\u05E9\u05E8\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8\
  \ \u05D0\u05D5 API \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05D1\
  \u05D5\u05E8 \u05E9\u05DC\u05D9\u05D7\u05D4 \u05D0\u05D5 \u05D0\u05D9\u05E1\u05D5\
  \u05E3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05E9\u05D9\u05E8\u05D5\u05EA\
  \u05D9 \u05E8\u05E9\u05EA,\u2026"
lastmod: 2024-02-19 22:04:57.820127
model: gpt-4-0125-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D1-Google\
  \ Apps Script \u05D4\u05D9\u05D0 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D1\u05E6\
  \u05E2 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05D9\
  \u05EA \u05DC\u05E9\u05E8\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D0\
  \u05D5 API \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05D1\u05D5\u05E8\
  \ \u05E9\u05DC\u05D9\u05D7\u05D4 \u05D0\u05D5 \u05D0\u05D9\u05E1\u05D5\u05E3 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05E8\
  \u05E9\u05EA,\u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP ב-Google Apps Script היא על מנת לבצע קריאה תוכניתית לשרת אינטרנט או API חיצוני. מתכנתים עושים זאת עבור שליחה או איסוף נתונים משירותי רשת, משלבים טווח רחב של משאבי רשת ופונקציונליות ישירות לתוך הפרויקטים שלהם ב-Google Apps Script.

## איך לעשות:

ב-Google Apps Script, הדרך העיקרית לשלוח בקשת HTTP היא באמצעות שימוש בשירות `UrlFetchApp`. שירות זה מספק שיטות לביצוע בקשות GET ו-POST ב-HTTP. הנה דוגמה פשוטה לביצוע בקשת GET כדי לאסוף נתוני JSON:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

עבור בקשת POST, שנעשית בדרך כלל כדי לשלוח נתונים לשרת, יש לכלול פרטים נוספים בפרמטר האפשרויות:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // המרת אובייקט ה-JavaScript למחרוזת JSON
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

הדוגמאות הללו מראות ביצועים בסיסיים של בקשות GET ו-POST. התוצאה תלויה בתגובת ה-API וניתן לצפות בה דרך יומן של Google Apps Script.

## צלילה עמוקה

שירות ה-`UrlFetchApp` של Google Apps Script התפתח מאוד מאז הוצג, ומציע שליטה מדויקת יותר על בקשות HTTP עם תכונות כמו הגדרת כותרות, payload, וטיפול ב-multipart/form-data להעלאת קבצים. למרות שהוא מספק אמצעי ישיר לשילוב שירותי רשת חיצוניים, מפתחים שמגיעים משפות צד שרת עשירות יותר עלולים למצוא את הפונקציונליות שלו מוגבלת בהשוואה לספריות כמו `requests` של Python או ה-API `fetch` של JavaScript ב-Node.js.

אחד המגבלות הבולטות הוא הגבלת זמן הביצוע עבור Google Apps Script, שמשפיעה על בקשות ארוכות מועד. נוסף על כך, למרות ש-`UrlFetchApp` מכסה טווח רחב של תרחישים, תרחישים מורכבים יותר הכוללים אימות OAuth או טיפול ב-payloads גדולים מאוד עשויים לדרוש פתרונות יצירתיים או שימוש במשאבי Google Cloud נוספים.

עם זאת, עבור רוב האינטגרציות שמפתחי Google Workspace נתקלים בהן – החל מאוטומטיזציה של איסוף נתונים ועד לפרסום עדכונים לשירותים חיצוניים – `UrlFetchApp` מספק כלי עוצמתי ונגיש. האינטגרציה שלו לתוך Google Apps Script מונעת צורך בספריות חיצוניות או התקנה מורכבת, הופכת את ביצוע בקשות HTTP לישירות יחסית תוך התמודדות עם המגבלות של Google Apps Script. ככל שנוף של API-ים ברשת ממשיך להתרחב, `UrlFetchApp` נשאר גשר קריטי עבור תכניות Google Apps Script לקיים אינטרקציה עם העולם מחוץ לאקוסיסטם של Google.
