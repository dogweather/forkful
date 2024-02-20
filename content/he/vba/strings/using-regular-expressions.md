---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:44.762557-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1-Visual Basic for Applications (VBA) \u05DE\u05E1\
  \u05E4\u05E7\u05D9\u05DD \u05D3\u05E8\u05DA \u05E2\u05D5\u05E6\u05DE\u05EA\u05D9\
  \u05EA \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9, \u05D4\u05EA\u05D0\u05DE\u05D4, \u05D5\
  \u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05DC\u05DE\u05E9\u05D9\
  \u05DE\u05D5\u05EA \u05DB\u05DE\u05D5\u2026"
lastmod: 2024-02-19 22:04:58.255925
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1-Visual Basic for Applications (VBA) \u05DE\u05E1\
  \u05E4\u05E7\u05D9\u05DD \u05D3\u05E8\u05DA \u05E2\u05D5\u05E6\u05DE\u05EA\u05D9\
  \u05EA \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9, \u05D4\u05EA\u05D0\u05DE\u05D4, \u05D5\
  \u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05DC\u05DE\u05E9\u05D9\
  \u05DE\u05D5\u05EA \u05DB\u05DE\u05D5\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D9\u05DC\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

ביטויים רגולריים (regex) ב-Visual Basic for Applications (VBA) מספקים דרך עוצמתית לחיפוש, התאמה, ומניפולציה של מחרוזות. מתכנתים משתמשים בהם למשימות כמו אימות נתונים, פרסור והמרה, בשל גמישותם ויעילותם בטיפול בתבניות מחרוזת מורכבות.

## איך ל:

כדי להשתמש בביטויים רגולריים ב-VBA, ראשית יש לאפשר את ספריית Microsoft VBScript Regular Expressions. בעורך ה-VBA, עברו אל `כלים` -> `הפניות`, ולאחר מכן סמנו את `Microsoft VBScript Regular Expressions 5.5`.

הנה דוגמה בסיסית למציאת האם תבנית קיימת בתוך מחרוזת:

```vb
Sub FindPattern()
    Dim regex As Object
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = True
        .Pattern = "\bis\b"  ' מחפש את המילה "is"
    End With
    
    Dim testString As String
    testString = "This is a test string."
    
    If regex.Test(testString) Then
        MsgBox "נמצאה תבנית."
    Else
        MsgBox "התבנית לא נמצאה."
    End If
End Sub
```

להחלפת תבנית במחרוזת:

```vb
Sub ReplacePattern()
    Dim regex As Object, replacedString As String
    Set regex = CreateObject("VBScript.RegExp")
    
    With regex
        .Global = True
        .IgnoreCase = False
        .Pattern = "\s"  ' מתאים לכל תו של רווח לבן
    End With
    
    replacedString = regex.Replace("This is a test string.", "_")
    MsgBox replacedString  ' מפיק: "This_is_a_test_string."
End Sub
```

## צלילה עמוקה

הכללת ביטויים רגולריים בשפות תכנות מצאה את עצמה לעיתים קרובות מחזורית לכלים של Unix משנות ה-70. VBA משלבת regex דרך ספריית VBScript Regular Expressions, מדגישה את חשיבותם במשימות עיבוד טקסט גם ביישומים שאינם מקושרים בדרך כלל למניפולציה כבדה של טקסט כמו Excel או Access.

למרות עוצמתם, השימוש בביטויים רגולריים ב-VBA לעיתים יכול להיות פחות אינטואיטיבי או ביצועי בהשוואה ליישומים יותר מודרניים בשפות כמו Python או JavaScript. לדוגמה, מודול ה-`re` של Python מציע תמיכה נרחבת לקבוצות עם שמות ותכונות התאמה סופיסטיקטיות יותר, מה שמספק גישה נקייה ופוטנציאלית נוחה יותר לקריאה. עם זאת, כשעובדים בתוך האקוסיסטם של VBA, ביטויים רגולריים נשארים כלי יקר ערך למשימות הדורשות התאמת תבניות או מניפולציה של טקסט. הפסד היעילות לעיתים קרובות זניח באור הנוחות והיכולות שביטויים רגולריים מביאים לשולחן כאשר מתמודדים עם מחרוזות ביישומי Office.
