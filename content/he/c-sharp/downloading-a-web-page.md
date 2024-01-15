---
title:                "הורדת עמוד אינטרנט"
html_title:           "C#: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה:
למה להתעסק בהורדת דף אינטרנט?
 במהלכי המאמר הזה, נלמד כיצד להשתמש בשפת תכנות C# כדי להוריד דף אינטרנט ולקבל גישה לתוכן שלו.

## איך לעשות זאת:
תחילה, נכיר את הספרייה System.Net המציעה כלים להתחברות רשת. נגדיר את המשתנה "url" ככתובת האתר המבוקש ונשתמש בפונקציית WebClient() כדי לבצע את הבקשה לכתובת זו. לבסוף, נדפיס את תוכן הדף המורד על ידי שימוש בפונקציה DownloadString() ובפקודת Console.WriteLine().

```C#
using System; 
using System.Net;

string url = "https://www.example.com";
WebClient webClient = new WebClient();
string downloadedPage = webClient.DownloadString(url);
Console.WriteLine(downloadedPage);
```

Output:
```
<!DOCTYPE html>
<html>
<head>
	<title>Example</title>
</head>
<body>
	<h1>Welcome to example.com</h1>
</body>
</html>
```

## לחקור יותר עמוק:
השתמשנו בפונקציה DownloadString() כדי להוריד דף אינטרנט, אך ישנן פונקציות אחרות בספרייה זו שמאפשרות לנו לשלוט בדרך הבוקשת במידת ויש לנו יותר מפרמטר אחד. פונקציה DownloadData() מאפשרת לנו להוריד גם קבצים אחרים מהאתר, כמו תמונות או קבצי טקסט, בעוד שפונקציה DownloadFile() מאפשרת לנו לשמור את הקובץ המורד בשם ובמיקום שאנו בוחרים.

## ראו גם:
- [תיעוד ה-System.Net במדריך הרשמי של C#](https://docs.microsoft.com/en-us/dotnet/api/system.net?view=netframework-4.8)
- [מדריך להורדת דף אינטרנט ב-C# עם WebRequest ו-WebClient](https://www.c-sharpcorner.com/article/download-web-page-in-C-Sharp/)