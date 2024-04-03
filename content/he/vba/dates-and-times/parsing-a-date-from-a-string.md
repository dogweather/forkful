---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:20.569305-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: VBA \u05DE\u05E6\u05D9\
  \u05E2\u05D4 \u05D3\u05E8\u05DA \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05E0\u05EA\
  \u05D7 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D4 `CDate` \u05D0\u05D5 \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4\
  \ `DateValue`. \u05E2\u05DD \u05D6\u05D0\u05EA, \u05D7\u05E9\u05D5\u05D1 \u05E9\u05D4\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05EA\u05D4\u05D9\u05D4 \u05D1\u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05D5\u05DB\u05E8. \u05D4\
  \u05E0\u05D4\u2026"
lastmod: '2024-03-13T22:44:39.074521-06:00'
model: gpt-4-0125-preview
summary: "VBA \u05DE\u05E6\u05D9\u05E2\u05D4 \u05D3\u05E8\u05DA \u05D9\u05E9\u05D9\
  \u05E8\u05D4 \u05DC\u05E0\u05EA\u05D7 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\
  \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `CDate` \u05D0\u05D5 \u05D4\u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D4 `DateValue`."
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## איך לעשות:
VBA מציעה דרך ישירה לנתח מחרוזת לתאריך באמצעות הפונקציה `CDate` או הפונקציה `DateValue`. עם זאת, חשוב שהמחרוזת תהיה בפורמט תאריך מוכר.

הנה דוגמה בסיסית באמצעות ה-`CDate`:

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Parsed Date: "; parsedDate
End Sub
```

אם תריץ את הקוד הזה, הפלט בחלון המיידי (ניתן לגישה דרך `Ctrl+G` בעורך VBA) יהיה:

```
Parsed Date: 4/1/2023 
```

בחלופה, תוכל להשתמש בפונקציה `DateValue`, שהיא ספציפית יותר לתאריכים (מתעלמת מהחלק הזמני):

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "April 1, 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Parsed Date using DateValue: "; parsedDate
End Sub
```

דוגמה לפלט עבור זה תהיה דומה בחלון המיידי:

```
Parsed Date using DateValue: 4/1/2023
```

חשוב לזכור שהצלחת הניתוח תלויה בכך שפורמט התאריך של המחרוזת תואם את הגדרות המערכת או היישום.

## צלילה עמוקה
באופן פנימי, כאשר VBA נותחת מחרוזת לתאריך, היא משתמשת בהגדרות האזוריות של מערכת ההפעלה Windows לפרשנות פורמט התאריך. זה מרכזי להבנה מפני שמחרוזת תאריך שנותחת באופן מושלם על מערכת אחת עלולה לגרום לשגיאה על אחרת אם הן משתמשות בהגדרות תאריך/שעה שונות.

בראשיתית, טיפול בתאריכים היה מקור נפוץ לבאגים ביישומים, במיוחד אלה שנמצאים בשימוש בינלאומי. התלות הזאת בהגדרות האזוריות ב-VBA היא הסיבה לכך שיש כאלה שעשויים לשקול חלופות כמו הפורמט ISO 8601 (למשל, "YYYY-MM-DD") לייצוג תאריך לא מבולבל ולניתוח ברחבי מערכות שונות. למרבה הצער, VBA לא תומכת באופן מובנה ב-ISO 8601, וניתוח ידני היה נדרש להתאמה מדויקת.

לטיפול בניתוח תאריכים מורכב מעבר למה ש`CDate` או `DateValue` יכולים לטפל, או כדי להבטיח ניתוח עקבי ללא תלות בהגדרות הלוקל של המערכת, מתכנתים עשויים להיעזר בפונקציות ניתוח מותאמות אישית. אלו עשויות לכלול פיצול של מחרוזת התאריך לרכיבים (שנה, חודש, יום) ובניית תאריך באמצעות הפונקציה `DateSerial`. אחרים עשויים לבחור בשפות או בספריות עוצמתיות יותר שתוכננו עם תכנון בינלאומי בראש עבור משימות כאלו.
