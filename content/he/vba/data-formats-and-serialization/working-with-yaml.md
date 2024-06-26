---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:41.880218-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E2\u05D1\u05D5\
  \u05D3\u05D4 \u05E2\u05DD YAML \u05D1-VBA \u05D3\u05D5\u05E8\u05E9\u05EA \u05D4\u05D1\
  \u05E0\u05D4 \u05E9\u05DC \u05DB\u05D9\u05E6\u05D3 \u05DC\u05E0\u05EA\u05D7 \u05D5\
  \u05DC\u05D4\u05DE\u05D9\u05E8 YAML \u05DC\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E9\
  -VBA \u05D9\u05DB\u05D5\u05DC \u05DC\u05EA\u05E4\u05E2\u05DC \u05D1\u05E7\u05DC\u05D5\
  \u05EA, \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05DE\u05D9\u05DC\u05D5\u05E0\
  \u05D9\u05DD \u05D0\u05D5 \u05D0\u05D5\u05E1\u05E4\u05D9\u05DD. \u05DC\u05E6\u05E2\
  \u05E8\u05E0\u05D5, VBA \u05D0\u05D9\u05E0\u05D5 \u05EA\u05D5\u05DE\u05DA \u05D1\
  \u05D0\u05D5\u05E4\u05DF\u2026"
lastmod: '2024-03-13T22:44:39.092824-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML \u05D1-VBA \u05D3\u05D5\
  \u05E8\u05E9\u05EA \u05D4\u05D1\u05E0\u05D4 \u05E9\u05DC \u05DB\u05D9\u05E6\u05D3\
  \ \u05DC\u05E0\u05EA\u05D7 \u05D5\u05DC\u05D4\u05DE\u05D9\u05E8 YAML \u05DC\u05E4\
  \u05D5\u05E8\u05DE\u05D8 \u05E9-VBA \u05D9\u05DB\u05D5\u05DC \u05DC\u05EA\u05E4\u05E2\
  \u05DC \u05D1\u05E7\u05DC\u05D5\u05EA, \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC\
  \ \u05DE\u05D9\u05DC\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05D0\u05D5\u05E1\u05E4\
  \u05D9\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך לעשות:
עבודה עם YAML ב-VBA דורשת הבנה של כיצד לנתח ולהמיר YAML לפורמט ש-VBA יכול לתפעל בקלות, בדרך כלל מילונים או אוספים. לצערנו, VBA אינו תומך באופן טבעי בניתוח או בסידור של YAML. עם זאת, ניתן להשתמש בשילוב של כלים להמרה ל-JSON ואובייקטים של מילון כדי לעבוד עם נתוני YAML, בהתחשב בקשר ההדוק ל-JSON.

ראשית, המירו את נתוני ה-YAML שלכם ל-JSON באמצעות ממיר מקוון או כלי המרת YAML ל-JSON בסביבת הפיתוח שלכם. לאחר ההמרה, תוכלו להשתמש בדוגמה הבאה לניתוח JSON ב-VBA, חשוב לציין שגישה זו מאפשרת באופן עקיף לעבוד עם YAML:

```vb
' הוסיפו הפניה ל-Microsoft Scripting Runtime למילון
' הוסיפו הפניה ל-Microsoft XML, v6.0 לניתוח JSON

Sub ParseYAMLAsJSON()
    Dim jsonText As String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' זהו JSON המומר מ-YAML
    
    ' בהנחה שיש לכם פונקציית ניתוח JSON
    Dim parsedData As Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Name: " & parsedData("name")
    Debug.Print "Age: " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText As String) As Dictionary
    ' מקום ללוגיקת ניתוח JSON - ייתכן שתשתמשו כאן בספרייה חיצונית
    Set JsonParser = New Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
בדוגמה זו, הפונקציה `JsonParser` משמשת כתחליף למקום שבו הייתם מנתחים את ה-JSON. קיימות ספריות שונות שיכולות לעזור בניתוח JSON, מאחר שספריות לניתוח YAML ישירות ל-VBA נדירות.

## חקירה עמוקה
היעדר טיפול ישיר ב-YAML ב-VBA ניתן לייחס לגילו ולסביבה שבה נבנה, שלא תוכננה במקור עם פורמטי סידור נתונים מודרניים בנפש. YAML עצמו התפתח כפורמט פופולרי לתצורה ולסידור נתונים בתחילת שנות ה-2000, במקביל לבוא באפליקציות שדרשו קבצי תצורה יותר ידידותיים לאדם.

תכנתים בדרך כלל מנצלים כלים או ספריות חיצוניות כדי לגשר על הפער בין VBA ל-YAML. זה לעיתים קרובות כולל המרה של YAML ל-JSON, כפי שהוצג, נוכח התמיכה ב-JSON הזמינה דרך ספריות שונות והדמיון בין JSON ל-YAML מבחינת המבנה והמטרה.

עבודה ישירה עם YAML ב-VBA מדגימה את גמישות השפה, אך ראוי לציין כי סביבות תכנות אחרות (למשל, Python או JavaScript) מספקות תמיכה יותר טבעית וחלקה ב-YAML. אלטרנטיבות אלו עשויות להתאים יותר לפרויקטים המסתמכים באופן כבד על YAML לתצורה או לסידור נתונים. עם זאת, למי שמחויבים ל-VBA או שהשימוש בו נדרש, השיטה העקיפה דרך המרת JSON נותרת גישה כדאית ושימושית לניהול ולתפעול נתוני YAML.
