---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# שולחים בקשת HTTP בשפת C# - המדריך המושלם

## מה זה ולמה? 

שליחת בקשת HTTP היא דרך שבה האפליקציה שלנו יכולה לדבר עם שרתים אחרים ברשת. הנתונים שאנחנו שולחים ומקבלים בתהליך זה ממש מאפשרים לנו להשתמש בקוד שלנו כדי ליצור אינטראקציות מורכבות עם העולם החיצוני.

## איך לעשות את זה:

בשפת C# משתמשים במחלקת HttpClient לשליחת בקשות HTTP. אז כך זה עובד:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

public class Example
{
    private static readonly HttpClient client = new HttpClient();

    public static async Task Main()
    {
        HttpResponseMessage response = await client.GetAsync("http://example.com");

        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();

        Console.WriteLine(responseBody);
    }
}
```

הקוד מבצע בקשה לכתובת http://example.com ולאחר מכן מדפיס את התוכן שהוא מקבל בתשובה.

## אין להתעמק

1. במהלך השנים, המחלקה HttpClient התפתחה לכדי הפתרון הסטנדרטי לביצוע בקשות HTTP ב-.NET, שעברה שיפורים רבים לאורך הדרך.

2. ישנם גם אלטרנטיבות אחרות, כמו RestSharp ו Flurl.Http, שהן אפשרויות נוספות ליצירת בקשות HTTP ב-C#.

3. שימו לב שהשימוש בHttpClient יכול להיות לחץ על מערכת הקשת לכך שהמחלקה הזו מנהלת מאגר מחזורי של חיבורים. זו הסיבה שנהוג לשמור על מופע יחיד של HttpClient באפליקציה.

## ראה גם: 

- [עזרה ל- HttpClient באתר הרשמי של Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)

- [תיעוד HTTP ב-C# מאתר StackOverflow](https://stackoverflow.com/questions/tagged/c%23+http)

קודים נעימים!