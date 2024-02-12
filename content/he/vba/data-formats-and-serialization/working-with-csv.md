---
title:                "עבודה עם קבצי CSV"
aliases:
- he/vba/working-with-csv.md
date:                  2024-02-01T22:06:25.303078-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם קבצי CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/vba/working-with-csv.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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
