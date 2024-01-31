---
title:                "ניתוח HTML"
date:                  2024-01-20T15:34:04.710203-07:00
simple_title:         "ניתוח HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פענוח HTML הוא תהליך שבו אנחנו לוקחים קוד HTML ומתרגמים אותו לצורה שבה ניתן לעבוד איתה בסקריפטים. תוכניתנים עושים את זה כדי לחלץ נתונים, לבדוק תקינות, ולאוטומט פעולות על דפי אינטרנט.

## איך לעשות:
כדי לפענח HTML בפאוורשל, אפשר להשתמש במודולים חיצוניים כמו `HtmlAgilityPack` או בפקודות פשוטות יותר כמו `Invoke-WebRequest`. נתחיל בדוגמה עם `Invoke-WebRequest`:

```PowerShell
# שולף את קוד ה-HTML של דף אינטרנט
$response = Invoke-WebRequest -Uri 'https://www.example.com'

# מוציא קטעים מסוימים מה-HTML באמצעות CSS selectors
$titles = $response.ParsedHtml.getElementsByTagName('h1')

# מדפיס כל כותרת
foreach ($title in $titles) {
  Write-Output $title.innerText
}
```

הפלט יהיה את טקסט הכותרות שנמצאים בתוך תגי `<h1>` מהדף שגישה אליו ביצענו.

## ניתוח עמוק
פאוורשל לא בנוי עם פענוח HTML בראש מעייניו. זה הסיבה שלעיתים אנו נזדקק למודולים כמו `HtmlAgilityPack`. בעבר, בפאוורשל הישן, פענוח ה-HTML היה מסורבל יותר עם קוד קומ ועזרים שונים כמו Internet Explorer COM object. כיום, עם פונקציות ברמת המערכת כמו `Invoke-WebRequest`, התהליך פשוט יותר, אבל עדיין לא מושלם.

האלטרנטיבה הפופולרית `HtmlAgilityPack` מתמודדת עם ה-HTML בצורה מורכבת יותר ומאפשרת אינטראקציה עם ה-HTML באופן ידידותי יותר למתכנת, דומה לעבודה בשפות כמו C# או Python בעזרת מודולים או ספריות מתאימות.

מלבד `HtmlAgilityPack`, ישנם כלים נוספים כמו `AngleSharp` שנועדו לדמות את ה-DOM בצורה אפקטיבית ויעילה בפלטפורמות .NET.

## ראה גם
- [`HtmlAgilityPack` GitHub repository](https://github.com/zzzprojects/html-agility-pack)
- [ניתוח תגי HTML באמצעות `Invoke-WebRequest`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [AngleSharp GitHub repository](https://github.com/AngleSharp/AngleSharp)
