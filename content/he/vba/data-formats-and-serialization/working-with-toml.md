---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:58.480824-07:00
description: "TOML, \u05E9\u05E2\u05D5\u05DE\u05D3 \u05DCTom's Obvious, Minimal Language,\
  \ \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E1\u05D3\u05E8\u05D9\u05D9\
  \u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05E0\u05DE\u05E6\u05D0 \u05D1\
  \u05E9\u05D9\u05DE\u05D5\u05E9 \u05E8\u05D1 \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D5\
  \u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05E0\u05E6\u05DC\u05D9\u05DD \u05D0\u05EA TOML \u05D1\u05E9\
  \u05DC \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA\u05D5 \u05D5\u05D4\u05DE\u05D9\u05E4\
  \u05D5\u05D9 \u05D4\u05E7\u05DC \u05D0\u05DC\u2026"
lastmod: '2024-03-13T22:44:39.098847-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u05E9\u05E2\u05D5\u05DE\u05D3 \u05DCTom's Obvious, Minimal Language,\
  \ \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E1\u05D3\u05E8\u05D9\u05D9\
  \u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05E0\u05DE\u05E6\u05D0 \u05D1\
  \u05E9\u05D9\u05DE\u05D5\u05E9 \u05E8\u05D1 \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D5\
  \u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05E0\u05E6\u05DC\u05D9\u05DD \u05D0\u05EA TOML \u05D1\u05E9\
  \u05DC \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA\u05D5 \u05D5\u05D4\u05DE\u05D9\u05E4\
  \u05D5\u05D9 \u05D4\u05E7\u05DC \u05D0\u05DC\u2026"
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD TOML"
---

{{< edit_this_page >}}

## מה ולמה?

TOML, שעומד לTom's Obvious, Minimal Language, הוא פורמט סדריית נתונים שנמצא בשימוש רב עבור קובצי תצורה. מתכנתים מנצלים את TOML בשל קריאותו והמיפוי הקל אל מבני נתונים, מה שמאפשר תצורת יישומים בקלות במגוון סביבות תכנות, כולל Visual Basic for Applications (VBA).

## איך ל:

עבודה עם TOML ב-VBA כוללת ניתוח קובץ TOML לקריאת הגדרות או תצורות לתוך פרויקט VBA שלך. ל-VBA אין תמיכה פנימית עבור TOML, אז בדרך כלל תשתמשו במנתח או תמירו נתוני TOML לפורמט ש-VBA יכול לעבוד איתו בקלות, כמו JSON או XML. הנה איך לנתח באופן ידני קובץ תצורה פשוט של TOML:

1. **דוגמת קובץ TOML** (`config.toml`):
```
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **קוד VBA לניתוח TOML**:

בהנחה שתוכן TOML נקרא לתוך משתנה מחרוזת `tomlStr`, הקוד VBA הבא מדגים גישה פשטנית לניתוח הסעיף `[database]`:

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 And InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End If
    Next i
    
    'דוגמה לגישה לנתונים שננתחו
    Debug.Print "שרת המסד נתונים: "; config("database")("server")
End Function
```

3. **דוגמת פלט** (חלון מיידי):
```
שרת המסד נתונים: 192.168.1.1
```

## צלילה עמוקה

הקבלה המעשית של TOML בקהילת המפתחים מציגה מגמה לעבר קובצי תצורה פשוטים יותר, קריאים לאדם, בניגוד ל-XML ששלט פעם. פילוסופיית העיצוב של TOML מדגישה סמנטיקה ברורה ומכוונת לניתוח ישר ופשוט עם מינימום מעטפת. ב-VBA, טיפול ישיר ב-TOML כרוך בניתוח ידני או בהשתמשות בכלים חיצוניים להמרת TOML לפורמט ידידותי יותר ל-VBA מאחר ואין תמיכה רשמית. עם זאת, בעוד ששיטת הניתוח הידנית הזו מציגה גישה יסודית, שימוש בספריות חיצוניות או בפורמטים מתווכים כמו JSON עשוי להציע אסטרטגיות ניתוח חזקות ופחות שגויות יותר. בהתחשב באינטגרציה הרחבה של VBA עם Microsoft Office, המרה של TOML לJSON והשימוש ביכולות הניתוח הפנימיות של VBA ל-JSON (כאשר הדבר אפשרי) או במנתחי JSON חיצוניים, עשויה להוות זרם עבודה יעיל יותר. כמו כן, עם ההתפתחות המתמדת של פורמטים לסדריית נתונים, מתכנתים צריכים גם לשקול את YAML, שכמו TOML, מדגיש קריאות אנושית אך מציע פשרות שונות במונחי סיבוכיות וגמישות.
