---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:30.265892-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Visual Basic\
  \ for Applications (VBA), \u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD \u05D9\u05DB\u05D5\
  \u05DC \u05DC\u05D4\u05D9\u05D5\u05EA \u05E4\u05D7\u05D5\u05EA \u05D9\u05E9\u05D9\
  \u05E8 \u05DC\u05E2\u05D5\u05DE\u05EA \u05E9\u05E4\u05D5\u05EA \u05E2\u05DD \u05EA\
  \u05DE\u05D9\u05DB\u05D4 \u05D8\u05D1\u05E2\u05D9\u05EA \u05D1\u05D4\u05DD. \u05E2\
  \u05DD \u05D6\u05D0\u05EA, \u05E0\u05D9\u05EA\u05DF \u05DC\u05E0\u05D4\u05DC \u05E4\
  \u05E2\u05D5\u05DC\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.047903-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Visual Basic for Applications (VBA), \u05D8\u05D9\u05E4\u05D5\u05DC\
  \ \u05D1\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05D9\u05D5\u05EA \u05E4\u05D7\u05D5\
  \u05EA \u05D9\u05E9\u05D9\u05E8 \u05DC\u05E2\u05D5\u05DE\u05EA \u05E9\u05E4\u05D5\
  \u05EA \u05E2\u05DD \u05EA\u05DE\u05D9\u05DB\u05D4 \u05D8\u05D1\u05E2\u05D9\u05EA\
  \ \u05D1\u05D4\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך לעשות:
ב-Visual Basic for Applications (VBA), טיפול במספרים מרוכבים יכול להיות פחות ישיר לעומת שפות עם תמיכה טבעית בהם. עם זאת, ניתן לנהל פעולות מורכבות על ידי יצירת פונקציות או שימוש בפונקציות ספרייתיות קיימות. בואו נחקור דוגמה בסיסית של חיבור, חיסור, כפל, וחילוק של מספרים מרוכבים:

```vb
' פונקציה לחיבור מספרים מרוכבים
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' חילוץ החלקים הממשיים והמדומים מהמספרים המרוכבים
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' ביצוע החיבור
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' דוגמת שימוש
Sub ExampleUsage()
    Dim result As String
    result = AddComplex("3+2i", "1+7i")
    Debug.Print "תוצאת החיבור: " & result  ' פלט: תוצאת החיבור: 4+9i
End Sub
```

למרות שזה מדגים חיבור, ניתן להתאים גישות דומות לחיסור, כפל וחילוק. לפעולות מורכבות מעבר לאריתמטיקה בסיסית, ייתכן ששווה לחקור ספריות חיצוניות או לשלב פתרונות אחרים שתומכים בפעולות עם מספרים מרוכבים בצורה יותר טבעית.

## צלילה עמוקה:
VBA אינו כולל תמיכה מובנית למספרים מרוכבים, היבט בו היא נופלת מאחורי שפות כמו Python, שיש לה מחלקה למספרים מרוכבים (`complex`) או C++ עם ה-Sandard Template Library שלה (`std::complex`). באופן היסטורי, הצורך להתמודד עם מספרים מרוכבים ישירות ב-VBA הוא די נדיר, מאחר והיא לרוב משמשת לאוטומציה, ניהול אפליקציות של Office, ומשימות שבדרך כלל לא דורשות חישובים מתמטיים מורכבים. כאשר VBA נוצרה ופותחה, השימושים העיקריים שלה היו ממוקדים ביישומי עסקים ולא בחישובים מדעיים, מה שעשוי להסביר את ההשמטה.

למשימות שדורשות טיפול נרחב במספרים מרוכבים, מתכנתים עשויים למצוא את השימוש בשפה עם נטייה מתמטית יותר כמועיל. עם זאת, לאלו המחויבים או מוגבלים לשימוש ב-VBA, כתיבת פונקציות מותאמות אישית (כפי שהוצג) או שילוב עם תוכנה שיש לה את היכולות הללו (כמו MATLAB או אקסל עצמו במידה מסוימת) הם דרכים קדימה אפשריות. למרות המגבלות, פתרונות יצירתיים ושילובים חיצוניים יכולים להרחיב את שימושיות VBA לתחומים שלא תוכננה במקור עבורם, כולל עבודה עם מספרים מרוכבים.
