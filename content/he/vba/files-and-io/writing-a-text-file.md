---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:18.733131-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: VBA \u05DE\u05E6\u05D9\
  \u05E2\u05D4 \u05DE\u05E1\u05E4\u05E8 \u05E9\u05D9\u05D8\u05D5\u05EA \u05DC\u05DB\
  \u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5, \u05D0\u05DA \u05D0\u05D7\u05EA\
  \ \u05D4\u05D3\u05E8\u05DB\u05D9\u05DD \u05D4\u05D1\u05E8\u05D5\u05E8\u05D5\u05EA\
  \ \u05D5\u05D4\u05E4\u05E9\u05D5\u05D8\u05D5\u05EA \u05D1\u05D9\u05D5\u05EA\u05E8\
  \ \u05D4\u05D9\u05D0 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1-`FileSystemObject`. \u05DC\
  \u05D4\u05DC\u05DF \u05DE\u05D3\u05E8\u05D9\u05DA \u05E6\u05E2\u05D3 \u05D0\u05D7\
  \u05E8 \u05E6\u05E2\u05D3 \u05DC\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\
  \u05E5 \u05D8\u05E7\u05E1\u05D8\u2026"
lastmod: '2024-03-13T22:44:39.089492-06:00'
model: gpt-4-0125-preview
summary: "VBA \u05DE\u05E6\u05D9\u05E2\u05D4 \u05DE\u05E1\u05E4\u05E8 \u05E9\u05D9\
  \u05D8\u05D5\u05EA \u05DC\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5\
  , \u05D0\u05DA \u05D0\u05D7\u05EA \u05D4\u05D3\u05E8\u05DB\u05D9\u05DD \u05D4\u05D1\
  \u05E8\u05D5\u05E8\u05D5\u05EA \u05D5\u05D4\u05E4\u05E9\u05D5\u05D8\u05D5\u05EA\
  \ \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D4\u05D9\u05D0 \u05E9\u05D9\u05DE\u05D5\u05E9\
  \ \u05D1-`FileSystemObject`."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D1\u05E5 \u05D8\
  \u05E7\u05E1\u05D8"
weight: 24
---

## איך לעשות:
VBA מציעה מספר שיטות לכתיבת קובץ, אך אחת הדרכים הברורות והפשוטות ביותר היא שימוש ב-`FileSystemObject`. להלן מדריך צעד אחר צעד ליצירת קובץ טקסט פשוט ולכתיבת נתונים בו:

1. **הפניה ל-Microsoft Scripting Runtime**: ראשית, וודא שעורך ה-VBA שלך יכול לגשת ל-`FileSystemObject`. עבור אל Tools > References בעורך ה-VBA וסמן "Microsoft Scripting Runtime."

2. **יצירת קובץ טקסט**: קטע הקוד הבא ב-VBA מדגים איך ליצור קובץ טקסט ולכתוב שורה של טקסט לתוכו.

```vb
Sub WriteToFile()
    Dim fso As FileSystemObject
    Set fso = New FileSystemObject
    
    Dim textFile As Object
    ' פרמטרים ל-CreateTextFile: (שם הקובץ, החלפה, יוניקוד)
    Set textFile = fso.CreateTextFile("C:\yourPath\example.txt", True, False)
    
    ' כתיבת שורת טקסט
    textFile.WriteLine "Hello, VBA!"
    
    ' סגירת הקובץ
    textFile.Close
End Sub
```

סקריפט זה יוצר (או מחליף אם כבר קיים) קובץ בשם `example.txt` במדריך שצוין וכותב לתוכו "Hello, VBA!" לפני שסוגר את הקובץ לשמירת השינויים.

3. **דוגמא לפלט**:

לאחר הרצת סקריפט ה-VBA שלעיל, תמצאו קובץ בשם `example.txt` עם התוכן הבא:

```
Hello, VBA!
```

## ניתוח עמוק:
ה-`FileSystemObject` (FSO), חלק מספריית Microsoft Scripting Runtime, מספק ערכה עשירה של תכונות ושיטות לפעולות עם קבצים, המרחיבה מעבר למה שהטיפול הקובצים המסורתי של VBA מציע (למשל, `Open`, `Print #`, `Write #`). מלבד טיפול בקבצים, FSO יכול גם לנהל תיקיות וכוננים, הופכים אותו לכלי עוצמתי לפעולות מערכת הקבצים בתוך VBA.

עם זאת, כדאי לציין שלמרות ש-FSO מציע גישה מודרנית יותר לפעולות עם קבצים ב-VBA, הוא עלול להכניס עודף עבודה עבור משימות פשוטות בהשוואה למשפטי טיפול קבצים מובנים של VBA. יתר על כן, מאחר ש-FSO הוא חלק מספרייה חיצונית, ניידות ותאימות עם מערכות אחרות (למשל, גרסאות קודמות של Office, Office ל-Mac) עשויות להיות שאלות הדורשות מענה.

בהקשרים בהם ביצועים, תאימות או תלות מינימלית בתלות חיצוניות הן קריטיות, מתכנתים עשויים לשקול להשתמש בטכניקות טיפול בקבצים המובנות של VBA. עם זאת, עבור פעולות מורכבות יותר או כאשר עובדים בסביבה בה חששות אלו מופחתים (כמו בהגדרה תאגידית מבוקרת), היתרונות של FileSystemObject לעיתים קרובות עולים על החסרונות שלו.
