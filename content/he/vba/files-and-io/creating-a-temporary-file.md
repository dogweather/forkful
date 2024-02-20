---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:31.480231-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05D1-Visual Basic for Applications (VBA) \u05DB\u05E8\u05D5\u05DB\
  \u05D4 \u05D1\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05D9 \u05DC\u05E9\u05D9\
  \u05DE\u05D5\u05E9 \u05DC\u05D8\u05D5\u05D5\u05D7 \u05E7\u05E6\u05E8, \u05D1\u05D3\
  \u05E8\u05DA \u05DB\u05DC\u05DC \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DB\u05D6\u05D9\u05DB\u05E8\u05D5\u05DF\
  \ \u05DE\u05D8\u05DE\u05D5\u05DF \u05D1\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA\u2026"
lastmod: 2024-02-19 22:04:58.310920
model: gpt-4-0125-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05D1-Visual Basic for Applications (VBA) \u05DB\u05E8\u05D5\u05DB\u05D4\
  \ \u05D1\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D1\u05D0\u05D5\
  \u05E4\u05DF \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05D9 \u05DC\u05E9\u05D9\u05DE\
  \u05D5\u05E9 \u05DC\u05D8\u05D5\u05D5\u05D7 \u05E7\u05E6\u05E8, \u05D1\u05D3\u05E8\
  \u05DA \u05DB\u05DC\u05DC \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D0\u05D5 \u05DB\u05D6\u05D9\u05DB\u05E8\u05D5\u05DF \u05DE\
  \u05D8\u05DE\u05D5\u05DF \u05D1\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA\u2026"
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת קובץ זמני ב-Visual Basic for Applications (VBA) כרוכה ביצירת קובץ באופן תוכניתי לשימוש לטווח קצר, בדרך כלל לעיבוד נתונים או כזיכרון מטמון במשימות אוטומציה. מתכנתים עושים זאת כדי לנהל נתונים שאין צורך לאחסן לטווח ארוך, מה שמפחית את העומס ומבטיח יעילות בשימוש בזיכרון.

## איך לעשות:

ב-VBA, ניתן ליצור קובץ זמני באמצעות השימוש ב-`FileSystemObject` שזמין בספריית Microsoft Scripting Runtime. אובייקט זה מספק שיטות ליצירה, קריאה, כתיבה ומחיקה של קבצים ותיקיות. להלן מדריך צעד אחר צעד ליצירת קובץ זמני:

1. **הפעלת Microsoft Scripting Runtime**: ראשית, ודאו שההפניה ל-Microsoft Scripting Runtime מופעלת בסביבת ה-VBA שלכם. עברו אל כלים > הפניות בעורך ה-VBA, וסמנו "Microsoft Scripting Runtime".

2. **יצירת קובץ זמני**: הקוד הבא ב-VBA מדגים כיצד ליצור קובץ זמני בתיקיית הזמנים המוגדרת כברירת מחדל.

```vb
Sub CreateTemporaryFile()
    Dim fso As Object
    Dim tmpFile As Object
    
    'יצירת FileSystemObject
    Set fso = CreateObject("Scripting.FileSystemObject")
    
    'קבלת הנתיב של תיקיית הזמנים
    Dim tempFolder As String
    tempFolder = fso.GetSpecialFolder(2) '2 מציין את תיקיית הזמנים
    
    'יצירת קובץ זמני וקבלת התייחסות אליו
    Set tmpFile = fso.CreateTextFile(tempFolder & "\myTempFile.txt", True)
    
    'כתיבת משהו לקובץ
    tmpFile.WriteLine "זו בדיקה."
    
    'סגירת הקובץ
    tmpFile.Close
    
    'אופציונלית, הדפסת הנתיב להתייחסות
    Debug.Print "נוצר קובץ זמני ב: " & tempFolder & "\myTempFile.txt"
End Sub
```

3. **פלט לדוגמה**: כאשר אתם מריצים את הקוד הנ"ל, הוא יוצר קובץ זמני בשם `myTempFile.txt` בתיקיית הזמנים וכותב שורת טקסט אליו. אם לכם פתוחה החלונית המיידית (`Ctrl + G` בעורך ה-VBA), תראו:
   
```
נוצר קובץ זמני ב: C:\Users\[שם המשתמש שלכם]\AppData\Local\Temp\myTempFile.txt
```

## צלילה עמוקה

השיטה המוצגת משתמשת ב-`FileSystemObject` (FSO), חלק מ-Microsoft Scripting Runtime. FSO הוא כלי חזק לניהול מערכת הקבצים, שהוצג עם מהדורת התסריט של Visual Basic. למרות גילו, הוא עדיין נמצא בשימוש נרחב ב-VBA בשל פשטותו ורחבת התכונות שלו.

יצירת קבצים זמניים משחקת תפקיד קריטי במשימות תכנות ותסריטאות רבות, מספקת אזור חול לבדיקות או סביבת עבודה לתהליכים שאינם דורשים אחסון קבוע. עם זאת, מפתחים צריכים להתייחס לקבצים אלו בזהירות, לוודא שהם מוסרים או מנקים כאשר הם כבר אינם נדרשים, כדי למנוע דליפות נתונים לא רצויות או שימוש בלתי נחוץ במקום בדיסק.

למרות ש-VBA מספק שיטות מובנות לטיפול בקבצים ובתיקיות, ה-`FileSystemObject` מציע גישה יותר מונחית אובייקטים, שעשויה להיות יותר מוכרת למתכנתים הבאים משפות אחרות. עם זאת, טכנולוגיות או שפות חדשות עשויות להציע שיטות עמידות או בטוחות יותר לטיפול בקבצים זמניים, כמו שימוש במבני נתונים בזיכרון או ספריות קבצים זמניים מתמחות בסביבות כמו Python או .NET. במקרים אלו, למרות ש-VBA יכול לשמש היטב למשימות מהירות או לשילוב בתוך יישומי Office, רצוי לבחון אלטרנטיבות ליישומים נרחבים יותר או כאלו הדורשים רמת אבטחה גבוהה יותר.
