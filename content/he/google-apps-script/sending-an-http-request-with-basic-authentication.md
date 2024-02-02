---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-02-01T22:03:01.869487-07:00
model:                 gpt-4-0125-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/google-apps-script/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי כוללת קידוד שם משתמש וסיסמא בכותרת הבקשה לצורך גישה למשאבים מוגנים. מתכנתים משתמשים בשיטה זו לאימות מצד השרת, לשם אינטגרציה עם ממשקי API שדורשים אימות בסיסי לפעולות כמו אחזור נתונים או פרסום תוכן.

## איך לעשות:

ב-Google Apps Script, לצורך שליחת בקשת HTTP עם אימות בסיסי, יש להשתמש בשירות `UrlFetchApp` בשילוב עם כותרת אישור מקודדת ב-base64. הנה מדריך צעד אחר צעד:

1. **קידוד הזהויות**: תחילה, יש לקודד את שם המשתמש והסיסמה ב-base64. ל-Google Apps Script אין פונקציה יילידית לקידוד base64 של מחרוזות, לכן תשתמשו ב-Utilities.base64Encode למטרה זו.

```javascript
var username = 'YourUsername';
var password = 'YourPassword';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **הכנת אפשרויות הבקשה**: עם הזהויות המקודדות מוכנות, הכינו את אובייקט האפשרויות לבקשת ה-HTTP, כולל השיטה והכותרות.

```javascript
var options = {
  method: 'get', // או 'post', 'put', תלוי בצרכים שלכם
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // ניתן להוסיף כאן אפשרויות נוספות כמו 'muteHttpExceptions' לטיפול בשגיאות
};
```

3. **ביצוע הבקשה**: השתמשו בשיטת `UrlFetchApp.fetch` עם ה-URL המבוקש ואובייקט האפשרויות.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

פלט לדוגמא עבור בקשה מוצלחת ישתנה בהתאם לתגובת ה-API. במקרה של API מבוסס JSON, ייתכן שתראו משהו כמו:

```
{"status":"Success","data":"Resource data here..."}
```

וודאו שאתם מטפלים בשגיאות HTTP אפשריות על ידי בדיקת קוד התגובה או שימוש באפשרות `muteHttpExceptions` לניהול שגיאות מבוקר יותר.

## צלילה עמוקה

שליחת בקשת HTTP עם אימות בסיסי הייתה שיטה סטנדרטית בשפות תכנות רבות לגישה למשאבים מבוססי אינטרנט הדורשים אימות. בהקשר של Google Apps Script, `UrlFetchApp` מספקת דרך ישירה לבצע בקשות HTTP אלו, כולל אלו הדורשות אימות. הכללת זהויות בסיסיות בכותרות הבקשה היא שיטה פשוטה אך יעילה, אך באה עם חששות אבטחה, בעיקר מאחר והזהויות נשלחות בטקסט גלוי, רק מקודדות ב-base64, שניתן לפענח בקלות אם הן נתפסות.

לשם שיפור האבטחה, מומלצות חלופות כמו OAuth 2.0, במיוחד כאשר מתמודדים עם נתונים או פעולות רגישות. ל-Google Apps Script יש תמיכה מובנית ב-OAuth 2.0 עם ספריית ה-`OAuth2`, המפשטת את תהליך האימות מול שירותים התומכים בפרוטוקול זה.

למרות מגבלות האבטחה שלה, האימות הבסיסי עדיין משמש רחבות עבור אפליקציות פשוטות או פנימיות שאינן חשופות לאינטרנט הרחב. הוא פשוט ליישום, כיוון שהוא דורש רק בקשה יחידה עם כותרות מוגדרות כראוי, ולכן מהווה אופציה מושכת לאינטגרציות מהירות או עבור API-ים שבהם שיטות אבטחה גבוהות יותר אינן זמינות. עם זאת, מתכנתים מוזהרים לשקול את ההשלכות האבטחתיות ולחפש חלופות בטוחות יותר כאשר הן זמינות.
