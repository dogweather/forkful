---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:40.103780-07:00
description: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05D1-Visual Basic for Applications\
  \ (VBA) \u05DB\u05E8\u05D5\u05DA \u05D1\u05D7\u05D9\u05DC\u05D5\u05E5 \u05DE\u05D9\
  \u05D3\u05E2 \u05DE\u05E1\u05D5\u05D9\u05DD \u05DE\u05DE\u05E1\u05DE\u05DA HTML.\
  \ \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D5\u05D8\u05DE\u05D8 \u05D0\u05EA\
  \ \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E7\u05E8\u05D9\u05D0\u05D4\
  \ \u05D5\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\u05DD \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD\u2026"
lastmod: '2024-02-25T18:49:37.292872-07:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05D1-Visual Basic for Applications\
  \ (VBA) \u05DB\u05E8\u05D5\u05DA \u05D1\u05D7\u05D9\u05DC\u05D5\u05E5 \u05DE\u05D9\
  \u05D3\u05E2 \u05DE\u05E1\u05D5\u05D9\u05DD \u05DE\u05DE\u05E1\u05DE\u05DA HTML.\
  \ \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D5\u05D8\u05DE\u05D8 \u05D0\u05EA\
  \ \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E7\u05E8\u05D9\u05D0\u05D4\
  \ \u05D5\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\u05DD \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## מה ולמה?

פענוח HTML ב-Visual Basic for Applications (VBA) כרוך בחילוץ מידע מסוים ממסמך HTML. מתכנתים עושים זאת כדי לאוטמט את התהליך של קריאה והתמודדות עם נתונים מדפי אינטרנט, כמו לדוגמה גריפת תוכן מאתרי אינטרנט או אוטומציה של הגשות טפסים ואחזור נתונים, בתוך יישומים כמו Microsoft Excel או Access שתומכים ב-VBA.

## איך לעשות:

ב-VBA, ניתן לנתח HTML באמצעות `Microsoft HTML Object Library`. הוספת התייחסות לספרייה זו בעורך VBA שלך על ידי מעבר ל-Tools > References וסימון `Microsoft HTML Object Library`. זה מעניק לך גישה למחלקות לניווט ולשינוי מסמכי HTML.

הנה דוגמה פשוטה שמראה איך לטעון מסמך HTML מקובץ ולחלץ את כל הקישורים (תגיות עוגן):

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' טעינת תוכן HTML מקובץ
    htmlFile = "C:\path\to\your\file.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' הכנת מסמך HTML
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' קבלת כל תגיות העוגן
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' לולאה על כל אלמנטי העוגן והדפסת התכונה href
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

תסריט זה קורא את תוכן קובץ HTML, טוען אותו לתוך אובייקט `HTMLDocument`, מאחזר את כל אלמנטי העוגן (`<a>` tags), ולאחר מכן בוחן אותם, מדפיס את התכונה `href` של כל אחד לחלון Immediate.

## עיון עמוק:

בעבר, פענוח HTML ב-VBA היה מעט מסורבל בשל החסר בתמיכה ישירה בטכנולוגיות ניתוח אתרים מודרניות וטיפול במסמכים. ה-Microsoft HTML Object Library, למרות שהוא עוצמתי, נחשב למעט מיושן וייתכן שלא יתמודד עם תקני אינטרנט מודרניים באופן חלק כמו טכנולוגיות חדשות יותר.

למשימות מורכבות של ניתוח HTML וגריפת אתרים, כלים ושפות חלופיים כמו Python עם ספריות כמו Beautiful Soup או Scrapy מומלצים לעיתים קרובות. כלים מודרניים אלו מציעים גמישות רבה יותר, ביצועים טובים יותר, והם מתאימים יותר לתקני האינטרנט הנוכחיים. עם זאת, כאשר עובדים בתוך אקוסיסטם של Microsoft Office, שימוש ב-VBA עם ה-Microsoft HTML Object Library נותר מיומנות יקרת ערך. היא מאפשרת מניפולציה ישירה של תוכן HTML בדרך שמשתלבת בצורה חלקה עם יישומים כמו Excel ו-Access, מספקת שיטה ישירה לביצוע משימות הכרוכות בטיפול בסיסי במסמכי HTML ללא הצורך לצאת מהסביבה המוכרת של VBA.
