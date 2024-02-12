---
title:                "שימוש במערכים אסוציאטיביים"
aliases:
- /he/vba/using-associative-arrays/
date:                  2024-02-01T22:05:26.871444-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במערכים אסוציאטיביים"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/vba/using-associative-arrays.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

מערכים אסוציאטיווים, אשר לעיתים קרובות מוכרים כמילונים ב-Visual Basic for Applications (VBA), מאפשרים לתכנתים ליצור אוספים של זוגות מפתח-ערך. תכונה זו חשובה ביותר לאחסון ואיחזור יעיל של נתונים, ומציעה דרך גמישה ואינטואיטיבית יותר לניהול נתונים מאשר אינדקסים של מערכים מסורתיים.

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
