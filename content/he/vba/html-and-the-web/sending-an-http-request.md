---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:47.802042-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05DE\u05E4\
  \u05EA\u05D7 \u05DC\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP\
  \ \u05D1-VBA \u05D4\u05D5\u05D0 \u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E1\
  \u05E4\u05E8\u05D9\u05D9\u05EA `Microsoft XML, v6.0` (\u05D0\u05D5 \u05D2\u05E8\u05E1\
  \u05D0\u05D5\u05EA \u05D9\u05E9\u05E0\u05D5\u05EA \u05D9\u05D5\u05EA\u05E8, \u05EA\
  \u05DC\u05D5\u05D9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05E9\u05DC\u05DA). \u05E8\
  \u05D0\u05E9\u05D9\u05EA, \u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05E4\u05E0\u05D9\
  \u05D4 \u05D4\u05D6\u05D5 \u05DE\u05D5\u05E4\u05E2\u05DC\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.053067-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05DE\u05E4\u05EA\u05D7 \u05DC\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\
  \u05E7\u05E9\u05EA HTTP \u05D1-VBA \u05D4\u05D5\u05D0 \u05D4\u05E9\u05D9\u05DE\u05D5\
  \u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA `Microsoft XML, v6.0` (\u05D0\u05D5\
  \ \u05D2\u05E8\u05E1\u05D0\u05D5\u05EA \u05D9\u05E9\u05E0\u05D5\u05EA \u05D9\u05D5\
  \u05EA\u05E8, \u05EA\u05DC\u05D5\u05D9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05E9\
  \u05DC\u05DA)."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

## איך לעשות:
המפתח לשליחת בקשת HTTP ב-VBA הוא השימוש בספריית `Microsoft XML, v6.0` (או גרסאות ישנות יותר, תלוי במערכת שלך). ראשית, וודא שהפניה הזו מופעלת בפרויקט שלך על ידי מעבר ל-Tools > References בעורך ה-VBA וסימון `Microsoft XML, v6.0`.

הנה איך לשלוח בקשת HTTP GET פשוטה:

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

With httpRequest
    .Open "GET", "https://api.example.com/data", False
    .send
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Error: " & .Status & " - " & .statusText
    End If
End With
```

לגבי בקשת POST, כאשר אנו צריכים לשלוח מידע (למשל, JSON) לשרת:

```vb
Dim httpRequest As Object, postData As String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""key"":""value""}"

With httpRequest
    .Open "POST", "https://api.example.com/submit", False
    .setRequestHeader "Content-Type", "application/json"
    .send postData
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Error: " & .Status & " - " & .statusText
    End If
End With
```

פלט לדוגמא לבקשה מוצלחת עשוי להיות מחרוזת JSON או דף HTML, תלוי ב-API או בדף האינטרנט שאתה מתממשק איתו:

```
{"data": "This is the response from the server"}
```

## התעמקות
השיטה שהוצגה נעזרת באובייקט `MSXML2.XMLHTTP`, חלק מ-Microsoft XML Core Services (MSXML). היא הוצגה כדי להציע למפתחי VBA דרך לבצע פעולות מבוססות XML ועם הזמן הפכה לכלי נפוץ לבקשות HTTP, אפילו כאשר לא עובדים ישירות עם נתוני XML. למרות גילה, היא עדיין מהווה אופציה אמינה לאינטראקציות רשת פשוטות ב-VBA.

עם זאת, ל-VBA ולמנגנוני הבקשה ה-http שלו חסרה העמידות והגמישות הנמצאות בסביבות תכנות מודרניות. לדוגמא, טיפול בבקשות אסינכרוניות או עבודה בתוך יישומים הדורשים תכונות HTTP מתקדמות (כמו websockets או אירועים הנשלחים מהשרת) היא מחוץ להיקף של VBA. כאשר עובדים על פרויקטים מורכבים יותר של אינטגרציה ווב, מפתחים לעיתים נעזרים בספריות או כלים חיצוניים, או אף מאוטמטים התנהגות של דפדפן דרך טכניקות של web scraping, אך אלה הם פתרונות זמניים ולא מענים נכונים.

שפות וסביבות כמו Python עם ספריית ה-`requests` שלה או JavaScript הרצה על Node.js מציעות יכולות חזקות וגמישות יותר לבקשות HTTP ישירות מהקופסא, כולל פעולות אסינכרוניות, טיפול ב-JS
ON קל יותר, ותמיכה נרחבת בטכנולוגיות אינטרנט שונות. מפתחים הרחוקים במערכת האקולוגית של Microsoft יכולים לשקול מעבר ל-PowerShell או C# למשימות הדורשות אינטראקציה רשתית מתוחכמת יותר, תוך ניצול תכונות התכנות הרשתיות הנרחבות של .NET.

לפיכך, בעוד שיכולות הבקשת HTTP של VBA מספיקות לשאילתות פשוטות ומשימות איסוף נתונים, חקירת חלופות הופכת לקריטית ככל שדרישות הפרויקט שלך מתקדמות לעבר נוף האינטרנט המודרני והמורכב.
