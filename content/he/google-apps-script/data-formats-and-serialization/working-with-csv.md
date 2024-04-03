---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:48.597524-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9\
  \ CSV (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD\
  \ \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD) \u05D1-Google Apps Script \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4, \u05E9\u05D9\u05E0\u05D5\u05D9\
  \ \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E9\u05DC \u05E7\u05D1\u05E6\u05D9 \u05D8\
  \u05E7\u05E1\u05D8 \u05E4\u05E9\u05D5\u05D8\u05D9\u05DD \u05E9\u05D1\u05D4\u05DD\
  \ \u05DB\u05DC \u05E9\u05D5\u05E8\u05D4 \u05DE\u05D9\u05D9\u05E6\u05D2\u05EA \u05E8\
  \u05E9\u05D5\u05DE\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E2\u05DD \u05E2\
  \u05E8\u05DB\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.603399-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV\
  \ (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\
  \u05E4\u05E1\u05D9\u05E7\u05D9\u05DD) \u05D1-Google Apps Script \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4, \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D5\
  \u05DB\u05EA\u05D9\u05D1\u05D4 \u05E9\u05DC \u05E7\u05D1\u05E6\u05D9 \u05D8\u05E7\
  \u05E1\u05D8 \u05E4\u05E9\u05D5\u05D8\u05D9\u05DD \u05E9\u05D1\u05D4\u05DD \u05DB\
  \u05DC \u05E9\u05D5\u05E8\u05D4 \u05DE\u05D9\u05D9\u05E6\u05D2\u05EA \u05E8\u05E9\
  \u05D5\u05DE\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E2\u05DD \u05E2\u05E8\
  \u05DB\u05D9\u05DD \u05D4\u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\
  \u05E1\u05D9\u05E7\u05D9\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV"
weight: 37
---

## איך ל:


### קריאת נתוני CSV
כדי לקרוא נתוני CSV מקובץ האחסון ב-Google Drive, תחילה יש להשיג את תוכן הקובץ כמחרוזת, ולאחר מכן לנתח אותו. Google Apps Script מקלה על קבלת תוכן הקובץ באמצעות שירות DriveApp.

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // החלף במזהה הקובץ האמיתי
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // תיעוד תאי כל שורה
  }
}
```

### כתיבת נתוני CSV
יצירה וכתיבה ל-CSV כוללת בניית מחרוזת עם ערכים המופרדים בפסיקים ושורות חדשות, ולאחר מכן שמירה או ייצוא שלה. דוגמא זו מדגימה יצירת קובץ CSV חדש ב-Google Drive.

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // החלף עם מזהה התיקייה ב-Drive שבה יווצר הקובץ החדש
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### פלט לדוגמה
כאשר מתעדים תאי שורות מקריאת CSV:

```plaintext
[John, 29, Engineer]
[Jane, 34, Designer]
```

בעת הכתיבה, נוצר קובץ בשם "example.csv" עם התוכן:

```plaintext
Name,Age,Occupation
John Doe,29,Engineer
Jane Smith,34,Designer
```

## צלילה לעומק
מבחינה היסטורית, קבצי CSV זכו להעדפה בשל פשטותם ונגישותם לקריאה אנושית, ההופכת אותם לנגישים לא-מתכנתים ושימושיים למשימות בדיקת נתונים מהירות. עם זאת, Google Apps Script פועל במסגרת אקוסיסטם של Google, שבה Google Sheets משמשת כחלופה עוצמתית וידידותית למשתמש להתעסקות עם CSV. Sheets לא רק מספקת ממשק גרפי לעריכת נתונים אלא גם תומכת בנוסחאות מורכבות, עיצוב ועוד הרבה יכולות שחסרות ב-CSV הגולמי.

למרות היתרונות ש-Google Sheets מציעה, התעסקות ישירה עם CSV ב-Google Apps Script נותרת חשובה למשימות אוטומטיות, במיוחד כאשר מתמודדים עם מערכות חיצוניות שיוצרות או דורשות נתונים בפורמט CSV. לדוגמה, אינטגרציה עם מערכות ישנות, ייצוא נתונים לשימוש ביישומים אחרים, או עיבוד מקדים לפני זיקה ל-Google Sheets.

בנוסף, יכולתה של Google Apps Script לעבוד עם קבצי CSV יכולה להרחב באמצעות שירות ה-Utilities לצרכי קידוד מתקדמים, או להתמשק עם API-ים חיצוניים למשימות של המרה, ניתוח, או אימות. עם זאת, לעבודה עם מערכי נתונים גדולים או שדורשים מניפולציות מורכבות, שקול להשתמש ב-API-ים של Google Sheets או לחקור את BigQuery ליכולות עיבוד נתונים חזקות יותר.

למרות שהפשטות נותרת סיבה עיקרית לפופולריות של CSV, האלטרנטיבות האלו מציעות סט עשיר יותר של יכולות להתמודדות עם נתונים באקוסיסטם הרחב של Google Cloud.
