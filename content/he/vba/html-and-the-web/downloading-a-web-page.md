---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:37.727320-07:00
description: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D1-Visual Basic for Applications (VBA) \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05D0\u05EA \u05D4\u05E9\u05D2\u05EA \u05EA\u05D5\u05DB\u05DF HTML\
  \ \u05E9\u05DC \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05DE\u05D4\
  \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E8\u05D1\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05DE\u05E9\u05D9\
  \u05DE\u05D4 \u05D6\u05D5 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E2\u05D1\u05D3\
  \ \u05D0\u05D5 \u05DC\u05E0\u05EA\u05D7 \u05D0\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.056229-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8 \u05D1-Visual Basic for Applications (VBA) \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05D0\u05EA \u05D4\u05E9\u05D2\u05EA \u05EA\u05D5\u05DB\u05DF HTML \u05E9\
  \u05DC \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05DE\u05D4\u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E8\
  \u05D1\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05DE\u05E9\u05D9\u05DE\
  \u05D4 \u05D6\u05D5 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E2\u05D1\u05D3 \u05D0\
  \u05D5 \u05DC\u05E0\u05EA\u05D7 \u05D0\u05EA\u2026"
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05E2\u05DE\u05D5\u05D3 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8"
weight: 42
---

## מה ולמה?

הורדת דף אינטרנט ב-Visual Basic for Applications (VBA) כוללת את השגת תוכן HTML של דף אינטרנט מהאינטרנט. תכנתים רבים מבצעים משימה זו על מנת לעבד או לנתח את תוכן האתרים תכנותית, מתוך אקסל, אקסס או יישומי אופיס אחרים.

## איך לעשות:

כדי להוריד דף אינטרנט ב-VBA, ניתן להשתמש בספריית Microsoft XML, v6.0 (MSXML6), שמאפשרת בקשות HTTP לשרת. לפני שתתחילו בקוד, ודאו שהפניתם לספרייה זו בעורך VBA שלכם על ידי לכת "Tools" -> "References" וסימון "Microsoft XML, v6.0".

הנה דוגמה פשוטה לכיצד להוריד את תוכן HTML של דף אינטרנט:

```basic
Sub DownloadWebPage()
    Dim request As Object
    Dim url As String
    Dim response As String
    
    ' אתחול אובייקט הבקשה XML HTTP
    Set request = CreateObject("MSXML2.XMLHTTP")
    
    url = "http://www.example.com"
    
    ' פתח בקשה סינכרונית
    request.Open "GET", url, False
    
    ' שלח את הבקשה לשרת
    request.send
    
    ' קבל את תגובת הטקסט
    response = request.responseText
    
    ' הדפס את התגובה לחלון המיידי (לצורכי ניפוי באגים)
    Debug.Print response
    
    ' ניקוי
    Set request = Nothing
End Sub
```

הרצת תת-התוכנית הזו תדפיס את HTML של `http://www.example.com` אל חלון ה-"Immediate" בעורך VBA. שימו לב שהפרמטר `False` במתודת `Open` הופך את הבקשה לסינכרונית, כלומר הקוד יחכה עד שדף האינטרנט יורד לפני שימשיך לשורה הבאה.

## צלילה עמוקה

הטכניקה המוצגת נשענת על MSXML, מימוש של מיקרוסופט לתקן בקשות XML HTTP, המשמש לעיתים קרובות לבקשות AJAX בפיתוח אינטרנט. רכיב זה הוא חלק מזמן רב מערכת טכנולוגיות של מיקרוסופט, הופך אותו לבחירה איתנה לבקשות רשת ב-VBA.

עם זאת, התלות ב-MSXML ו-VBA להורדה וניתוח תוכן אינטרנט יכולה להיות מוגבלת, במיוחד עם אפליקציות אינטרנט מודרניות שמשתמשות בכובד רב ב-JavaScript לעיבוד תוכן דינמי. המגבלות הללו יכולות להפוך שפות או כלים אחרים כמו Python עם ספריות כמו BeautifulSoup או Selenium למתאימים יותר למשימות של שריטת אתרים, בשל היכולת שלהם לבצע JavaScript ולהתמודד עם אינטראקציות מורכבות באתרים.

למרות זאת, למשימות פשוטות שכוללות הורדת תוכן HTML ישיר או כאשר פועלים בהגבלות של יישומי אופיס, VBA נשארת כלי מעשי. האינטגרציה שלה בחבילת Office מאפשרת מניפולציה ישירה של מסמכים על סמך תוכן אינטרנט, מציעה יתרון ייחודי למקרים שימוש מסוימים.
