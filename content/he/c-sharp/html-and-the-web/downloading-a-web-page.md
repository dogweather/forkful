---
title:                "הורדת דף אינטרנט"
aliases:
- /he/c-sharp/downloading-a-web-page/
date:                  2024-01-20T17:43:59.074347-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט זה פעולה שבה תוכנת המחשב שלך משיגה את התוכן של דף ווב כדי שתוכל לעבוד איתו מקומית. תכנותים עושים זאת כדי לאסוף מידע, לבדוק זמינות של אתרים או ליצור גיבויים של דפי אינטרנט.

## איך לעשות:
ב-C# ניתן להוריד דף אינטרנט בקלות עם כמה שורות קוד. הנה דוגמה עם `HttpClient`:

```csharp
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        var url = "http://example.com"; // כתובת האתר להורדה
        using (var httpClient = new HttpClient())
        {
            try
            {
                string pageContent = await httpClient.GetStringAsync(url);
                Console.WriteLine(pageContent); // ידפיס את תכני הדף
            }
            catch (HttpRequestException e)
            {
                Console.WriteLine("Error downloading page: " + e.Message);
            }
        }
    }
}
```

פלט לדוגמה:

```
<!doctype html>
<html>
<head>
    <title>דוגמה לדף אינטרנט</title>
...
```

## צלילה לעומק:
היסטורית, להורדת דפי אינטרנט ב-C# השתמשו בכיתות כמו `WebClient` או `HttpWebRequest`, אבל `HttpClient` הפך לסטנדרט חדש עקב הממשק המודרני והיעיל יותר. בנוסף, `HttpClient` תומך ב-HTTP/2, אימות אוטומטי ובקרה טובה יותר על הרשת.

השימוש ב-`HttpClient` כרוך במספר דפוסים ומומלץ ליצור אובייקט אחד של `HttpClient` לשימוש מתמשך במקום ליצור אחד חדש עבור כל בקשה, כדי למנוע בעיות ביצועים שקשורות לפתיחת וסגירת חיבורים רבים.

## ראה גם:
- [Documentation for HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Best practices for using HttpClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient#examples)
- [Using HttpClientFactory for more complex scenarios](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/http-requests)
