---
title:                "עבודה עם JSON"
aliases:
- /he/vba/working-with-json/
date:                  2024-02-01T22:06:25.432100-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/vba/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

JSON (JavaScript Object Notation) הוא פורמט חילופי נתונים קל משקל שקל לקריאה ולכתיבה עבור בני אדם, ולניתוח וייצור עבור מכונות. מתכנתים משתמשים ב-JSON לשידור נתונים בין שרת לאפליקציית רשת או לאחסון מידע בצורה מובנית ונגישה בתוך מגוון סביבות תכנות, כולל Visual Basic for Applications (VBA).

## איך לעשות:

VBA לא תומך באופן טבעי בניתוח או ייצור של JSON, אז נשתמש בשפת סקריפט כמו JScript (דרך אובייקט ScriptControl) לניתוח מחרוזות JSON ובניית אובייקטים של JSON. הנה איך אפשר לנתח מחרוזת JSON ב-VBA:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Name: " & parsed.name & ", Age: " & parsed.age & ", City: " & parsed.city
End Sub
```

לייצור JSON, אפשר להשתמש בגישה דומה, בנייה של מחרוזת ה-JSON דרך שרשור:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## צלילה עמוקה

הגישות שהוצגו מנצלות את ScriptControl כדי לטפל ב-JJSON, כלומר, מעבירות את העבודה למנוע JavaScript. זווית יצירתית אך לא בהכרח הדרך היעילה או המודרנית ביותר לעבוד עם JSON בהקשר של VBA. ביישומים מורכבים יותר, שיטה זו עלולה להפוך למסורבלת ולהוסיף עלויות ביצוע או דאגות בטיחות, מכיוון ש-ScriptControl מתבצע בסביבה שיש לה גישה מלאה למחשב המארח.

סביבות תכנות אחרות, כמו Python או JavaScript, מציעות תמיכה מובנית ל-JJSON, הופכות אותן למתאימות יותר ליישומים שדורשים עיבוד JSON נרחב. שפות אלו מספקות ספריות מקיפות המקלות לא רק על ניתוח וייצור אלא גם על שאילתה ועיצוב של נתוני JSON.

למרות המגבלות הללו ב-VBA, הבנה של איך לעבוד עם JSON היא חיונית בעולם שבו חילופי נתונים מבוססי רשת וקבצי תצורה מתבצעים בעיקר בפורמט JSON. למתכנתי VBA, השליטה בטכניקות אלו פותחת הזדמנויות לשילוב עם ממשקי API של אתרי אינטרנט, לפרשנות של קבצי תצורה, או אפילו לבניית אפליקציות רשת פשוטות. עם זאת, כשפרויקטים גדלים במורכבות או דורשים ביצועים גבוהים, מפתחים עשויים לשקול לנצל סביבות תכנות ידידותיות יותר ל-JJSON.
