---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:25.303078-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9\
  \ CSV (Comma Separated Values, \u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\
  \u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD) \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05E7\u05D1\u05E6\u05D9\
  \ \u05D8\u05E7\u05E1\u05D8 \u05E4\u05E9\u05D5\u05D8\u05D9\u05DD \u05D0\u05D5 \u05DB\
  \u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4\u05DD, \u05E9\u05D1\u05D4\u05DD\
  \ \u05E9\u05D3\u05D5\u05EA \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05D5\
  \u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
lastmod: '2024-02-25T18:49:37.334029-07:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV\
  \ (Comma Separated Values, \u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\
  \u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD) \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05E7\u05D1\u05E6\u05D9 \u05D8\
  \u05E7\u05E1\u05D8 \u05E4\u05E9\u05D5\u05D8\u05D9\u05DD \u05D0\u05D5 \u05DB\u05EA\
  \u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4\u05DD, \u05E9\u05D1\u05D4\u05DD \u05E9\
  \u05D3\u05D5\u05EA \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05D5\u05E4\
  \u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV (Comma Separated Values, ערכים מופרדים בפסיקים) כוללת קריאה מקבצי טקסט פשוטים או כתיבה אליהם, שבהם שדות הנתונים מופרדים בפסיקים. מתכנתים לעיתים קרובות מבצעים משימה זו כדי להקל על חילופי נתונים בין יישומים תוכנתיים שונים, בהתחשב בפשטות ובאימוץ הרחב של תסדיר ה-CSV בסביבות תכנות שונות.

## איך ל:

Visual Basic for Applications (VBA) מפשטת את העבודה עם קבצי CSV באמצעות פונקציות ושיטות מובנות שמאפשרות בחלקלות קריאה מקבצים אלו וכתיבה אליהם. להלן דוגמאות הממחישות פעולות בסיסיות עם קבצי CSV.

### קריאת קובץ CSV:

```basic
Sub ReadCSV()
    Dim filePath As String
    filePath = "C:\example.csv"
    
    Open filePath For Input As #1
    
    Do Until EOF(1)
        Dim line As String
        Line Input #1, line
        Dim dataFields() As String
        dataFields = Split(line, ",")
        
        'לעבד את המערך dataFields כפי שנדרש
        Debug.Print Join(dataFields, ";") 'דוגמה לפלט המראה המרה מפסיקים לנקודה פסיק
    Loop
    
    Close #1
End Sub
```

### כתיבה לקובץ CSV:

```basic
Sub WriteCSV()
    Dim filePath As String
    filePath = "C:\output.csv"
    Dim dataToWrite As String
    dataToWrite = "ID,Name,Age" & vbCrLf & "1,John Doe,30" & vbCrLf & "2,Jane Doe,29"
    
    Open filePath For Output As #1
    Print #1, dataToWrite
    Close #1
End Sub
```

פלט לדוגמה ב-`output.csv`:
```
ID,Name,Age
1,John Doe,30
2,Jane Doe,29
```

## צלילה עמוקה

באופן היסטורי, קבצי CSV היו שיטה ישירה לאחסן נתוני טבלה בפורמט טקסט. הפשטות של מבנהו, שבו כל שורה מתאימה לרשומת נתונים אחת וכל שדה בתוך רשומה מופרד על ידי פסיק, היא גם חוזקו וגם מגבלתו של CSV. התסדיר לא תומך באופן טבעי בסוגי נתונים, מה שאומר שכל הנתונים מאוחסנים כמחרוזות, והעומס להמיר את הנתונים לסוג הנכון נופל על המתכנת.

ב-Visual Basic for Applications, העיסוק בקבצי CSV בעיקר נעשה דרך פעולות קובץ בסיסיות, כפי שהוצג בדוגמאות הקודמות. אין תמיכה ישירה בניתוח CSV כמו בשפות מודרניות יותר (למשל, המודול csv ב-Python), אשר מספקת שליטה ונוחות רבה יותר בעת התמודדות עם נתוני CSV.

עבור פעולות מורכבות יותר או עבודה עם קבצי CSV גדולים, מתכנתים עשויים למצוא חלופות טובות יותר מחוץ ל-VBA הטהורה, כגון שימוש בספריות חיצוניות או בשפות תכנות מצוידות ביכולות מתקדמות יותר לטיפול בקבצי CSV. עם זאת, למשימות פשוטות הקשורות לקבצי CSV, הגישה הישירה של VBA לעיתים קרובות מספיקה וקלה ליישום, ומציעה פתרון מהיר ליישומי Excel או לאוטומציה של תוכנות Microsoft Office אחרות.
