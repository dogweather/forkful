---
title:                "עבודה עם קבצי CSV"
aliases:
- he/google-apps-script/working-with-csv.md
date:                  2024-02-01T22:06:48.597524-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם קבצי CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/google-apps-script/working-with-csv.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV (ערכים מופרדים בפסיקים) ב-Google Apps Script כוללת קריאה, שינוי וכתיבה של קבצי טקסט פשוטים שבהם כל שורה מייצגת רשומת נתונים עם ערכים המופרדים בפסיקים. מתכנתים עושים זאת כדי להחליף נתונים בקלות בין יישומים שונים, מסדי נתונים או שפות תכנות בשל הקבלה הרחבה של CSV כתסדיר חילופי נתונים מבוסס טקסט פשוט.

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
