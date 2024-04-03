---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:26.871444-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-VBA, \u05D4\
  \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 `Dictionary` \u05DE\u05E1\u05E4\u05E7\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D3\
  \u05D5\u05DE\u05D4 \u05DC\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\
  \u05E6\u05D9\u05D0\u05D8\u05D9\u05D5\u05D5\u05D9\u05DD. \u05E2\u05DC \u05DE\u05E0\
  \u05EA \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5, \u05E2\u05DC\u05D9\u05DA\
  \ \u05E7\u05D5\u05D3\u05DD \u05DC\u05D4\u05D5\u05E1\u05D9\u05E3 \u05D4\u05E4\u05E0\
  \u05D9\u05D9\u05D4 \u05DC-Microsoft Scripting\u2026"
lastmod: '2024-03-13T22:44:39.046097-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-VBA, \u05D4\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 `Dictionary`\
  \ \u05DE\u05E1\u05E4\u05E7 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\
  \u05D9\u05D5\u05EA \u05D3\u05D5\u05DE\u05D4 \u05DC\u05DE\u05E2\u05E8\u05DB\u05D9\
  \u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D5\u05D5\u05D9\u05DD."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
weight: 15
---

## איך לעשות:
ב-VBA, האובייקט `Dictionary` מספק פונקציונליות דומה למערכים אסוציאטיווים. על מנת להשתמש בו, עליך קודם להוסיף הפנייה ל-Microsoft Scripting Runtime:

1. בעורך ה-VBA, עבור אל Tools > References...
2. סמן "Microsoft Scripting Runtime" ולחץ OK.

הנה איך להגדיר, למלא ולגשת לפריטים ב-`Dictionary`:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' הוספת פריטים
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' גישה לפריטים
Debug.Print sampleDictionary.Item("Name")  ' פלט: John Doe
Debug.Print sampleDictionary.Item("Age")   ' פלט: 29

' בדיקה אם מפתח קיים
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Occupation Key Exists"
End If

' הסרת פריטים
sampleDictionary.Remove("Occupation")

' לולאה דרך המילון
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## ניתוח עמוק
האובייקט `Dictionary` מתקשר מתחת לכיפה עם רכיבים של Windows Scripting Host. כך שהוא אובייקט COM מקושר מאוחר, שהיה דרך נפוצה להרחיב את פונקציונליות VBA בעבר. השימוש בו ב-VBA יכול לשפר משמעותית את יכולת השפה לנהל ערכות נתונים מורכבות מבלי לאכוף מבנה קשיח, כפי שנראה במערכים מסורתיים או בטווחים של Excel.

מגבלה אחת שכדאי לקחת בחשבון היא שגישה ל-`Dictionary` דורשת הגדרת הפנייה ל-Microsoft Scripting Runtime, אשר יכולה להסבך את הפצת פרויקטי VBA שלך. קיימות אלטרנטיבות כמו Collections בתוך VBA אך הן חסרות חלק מתכונות המפתח של ה-`Dictionary`, כגון היכולת לבדוק בקלות את קיומו של מפתח מבלי להפעיל שגיאה.

בהקשרים תכנותיים חדשים יותר, שפות כמו Python מציעות תמיכה מובנית למערכים אסוציאטיוויים (המכונים גם הם מילונים ב-Python) מבלי הצורך להוסיף הפניות חיצוניות. תמיכה זו המובנית מפשטת את התהליך ומציעה תכונות מתקדמות יותר מלכתחילה. עם זאת, במגבלות של VBA וליישומים ספציפיים המיועדים לאוטומציה של משימות בחבילת Microsoft Office, השימוש באובייקט `Dictionary` נשאר שיטה עוצמתית ורלוונטית למבני נתונים דמויי מערכים אסוציאטיווים.
