---
title:                "שליחת בקשת HTTP"
aliases:
- /he/google-apps-script/sending-an-http-request.md
date:                  2024-02-01T22:02:14.844798-07:00
model:                 gpt-4-0125-preview
simple_title:         "שליחת בקשת HTTP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/google-apps-script/sending-an-http-request.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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
