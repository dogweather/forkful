---
title:                "הורדת עמוד אינטרנט"
aliases: - /he/vba/downloading-a-web-page.md
date:                  2024-02-01T21:53:37.727320-07:00
model:                 gpt-4-0125-preview
simple_title:         "הורדת עמוד אינטרנט"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/vba/downloading-a-web-page.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
