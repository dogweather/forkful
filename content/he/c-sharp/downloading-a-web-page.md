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

## מה ולמה?
הורדת דף אינטרנט היא פעולה שנעשית כאשר מתכנתים משתמשים בקוד כדי להוריד דף אינטרנט מהאינטרנט למחשב שלהם. כך ניתן לגשת לתוכן של הדף ולעבוד עליו בצורה יעילה ונוחה. מתכנתים גם יכולים להשתמש בהורדת דף אינטרנט כדי לאסוף נתונים מאתרי אינטרנט או לבצע פעולות כמו עדכון תוכן אוטומטי.

## כיצד לעשות זאת:
הנה כמה דוגמאות להורדת דף אינטרנט בשפת C#:

```C#
using System;
using System.Net;

namespace SimpleWebDownloader
{
    class Program
    {
        static void Main(string[] args)
        {
            //קבלת נתוני הגבל הרשת הכללית
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12;

            //יצירת אובייקט WebClient והורדת תוכן הדף המבוקש
            WebClient client = new WebClient();
            string webpage = client.DownloadString("https://example.com");

            //הדפסת התוכן המורד על ידי WebClient
            Console.WriteLine(webpage);
        }
    }
}
```

תוצאה:
```
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
</head>
<body>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this
domain in literature without prior coordination or asking for permission.</p>
<p><a href="https://www.iana.org/domains/example">More information...</a></p>
</body>
</html>
```

## Deep Dive:
פעם קודמת, הורדת דף אינטרנט הייתה מתבצעת בעיקרון בעזרת תוכנות כמו wget או curl. אבל עם התפתחות שפות תכנות כמו C#, חשיבת ההורדה נכללה כחלק מתכנותי התוכנה בעקבות התפתחות מערכות האינטרנט החדשות. אם אתם מעוניינים לגשת לתוכן של דף אינטרנט, מכשיר מכשיר, אתר מכשיר או אתר ווב, הורדת דף אינטרנט היא דרך נוחה ופשוטה לעשות זאת.

אפשרויות אחרות להורדת דף אינטרנט כוללות שימוש ב-API או בספריות של צד שלישי כמו HtmlAgilityPack או AngleSharp. בכל זאת, שיטה הנפוצה ביותר והמומלצת היא להשתמש בספריה המובנית של .NET WebClient.

לעומת זאת, כשמשתמשים בספריה WebClient, יש לשים לב לאבטחת הרשת הכללית של .NET כדי לאפשר את ההורדה של דף אינטרנט עם כל מנגנון אבטחת רשת. בנוסף, כדי למנוע שגיאות בזמן ההורדה, כדאי לברור את פרוטוקול התקשורת מראש עם מחלקת ServicePointManager.

## ראו גם:
- [מדריך רשמי של מיקרוסופט על WebClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0)
- [המדריך המלא על הורדת דף אינטרנט בשפת C#](https://www.c-sharpcorner.com/article/downloading-web-page-data-in-c-sharp/)