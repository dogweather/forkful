---
title:                "רישום"
aliases:
- he/vba/logging.md
date:                  2024-02-01T21:57:29.448606-07:00
model:                 gpt-4-0125-preview
simple_title:         "רישום"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/vba/logging.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

התעדוף ב-Visual Basic for Applications (VBA) כולל רישום מידע אודות התנהגות זמן ריצה של תוכנית לקובץ, קונסולה, או מסד נתונים. מתכנתים משתמשים בתעדוף כדי לנטר את האפליקציות שלהם, לאבחן בעיות ולהבין את תכונות הביצועים.

## איך לעשות:

ב-VBA, אין מסגרת תעדוף מובנית כמו שנמצא בחלק מהשפות האחרות. עם זאת, מימוש מנגנון תעדוף פשוט הוא ישיר. להלן דוגמה של איך ליצור מתעד קבצים בסיסי.

1. **כתיבה לקובץ יומן**: פונקציית הדוגמה הזו, `LogMessage`, כותבת הודעות לקובץ טקסט עם חותמת זמן.

```basic
Sub LogMessage(message As String)
    Dim logFilePath As String
    Dim fileNum As Integer
    
    ' ציין את נתיב הקובץ של היומן
    logFilePath = ThisWorkbook.Path & "\log.txt"
    
    ' קבל את המספר הקובץ הזמין הבא
    fileNum = FreeFile()
    
    ' פתח את הקובץ להוספה
    Open logFilePath For Append As #fileNum
    
    ' כתוב את חותמת הזמן וההודעה של היומן
    Print #fileNum, Now & ": " & message
    
    ' סגור את הקובץ
    Close #fileNum
End Sub
```

כדי לתעד הודעה, פשוט קרא ל-`LogMessage("ההודעה שלך כאן")`. זה יפיק רשומות ב-*log.txt* כמו:

```
30/4/2023 3:45:32 PM: ההודעה שלך כאן
```

2. **קריאה מקובץ יומן**: לקרוא ולהציג את תוכן קובץ היומן:

```basic
Sub ReadLogFile()
    Dim logFilePath As String
    Dim fileContent As String
    Dim fileNum As Integer
    
    logFilePath = ThisWorkbook.Path & "\log.txt"
    fileNum = FreeFile()
    
    ' פתח את הקובץ לקריאה
    Open logFilePath For Input As #fileNum
    
    ' קרא את כל תוכן הקובץ
    fileContent = Input(LOF(fileNum), fileNum)
    
    ' סגור את הקובץ
    Close #fileNum
    
    ' הצג את תוכן הקובץ
    MsgBox fileContent
End Sub
```

## צלילה עמוקה

התעדוף ב-VBA, בשל היעדר מסגרת תעדוף ילידה, בדרך כלל מתבצע דרך פעולות קובץ בסיסיות או על ידי ניצול כוחם של אובייקטים חיצוניים של COM לצרכים מתקדמים יותר, כמו תעדוף למסד נתונים או יצירת אינטראקציה עם Windows Event Log. באופן היסטורי, התעדוף ב-VBA היה דרך לעקוף את המגבלות שמציבים כלי התיקון שגיאות והדיבאגינג הפשטניים שלו. אף על פי שהוא יעיל, ניהול קבצים ישיר למטרות תעדוף הוא פרימיטיבי ויכול להיות לא יעיל עם נפחי נתונים גדולים או תחת מקביליות רבה. ליכולות תעדוף יותר מתקדמות, מתכנתים לעיתים קרובות פונים לספריות חיצוניות או משתלבים עם מערכות שתוכננו במיוחד לתעדוף, כמו ערכת ELK (Elasticsearch, Logstash, Kibana) או Splunk, דרך קריאות שירות וב תו מסדי נתונים מתווכים. למרות ש-VBA אינו מציע את הנוחויות המודרניות שנמצאות בשפות תכנות חדשות יותר, הבנת יכולותיו והגבלותיו מאפשרת למתכנתים להשתמש בתעדוף ככלי חזק לניטור אפליקציות ואבחון.
