---
date: 2024-01-20 17:43:59.074347-07:00
description: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D6\u05D4 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4\
  \ \u05EA\u05D5\u05DB\u05E0\u05EA \u05D4\u05DE\u05D7\u05E9\u05D1 \u05E9\u05DC\u05DA\
  \ \u05DE\u05E9\u05D9\u05D2\u05D4 \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05E9\
  \u05DC \u05D3\u05E3 \u05D5\u05D5\u05D1 \u05DB\u05D3\u05D9 \u05E9\u05EA\u05D5\u05DB\
  \u05DC \u05DC\u05E2\u05D1\u05D5\u05D3 \u05D0\u05D9\u05EA\u05D5 \u05DE\u05E7\u05D5\
  \u05DE\u05D9\u05EA. \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E1\u05D5\u05E3\
  \ \u05DE\u05D9\u05D3\u05E2, \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D6\u05DE\u05D9\u05E0\
  \u05D5\u05EA \u05E9\u05DC\u2026"
lastmod: '2024-03-11T00:14:12.792433-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8 \u05D6\u05D4 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05EA\
  \u05D5\u05DB\u05E0\u05EA \u05D4\u05DE\u05D7\u05E9\u05D1 \u05E9\u05DC\u05DA \u05DE\
  \u05E9\u05D9\u05D2\u05D4 \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05E9\u05DC\
  \ \u05D3\u05E3 \u05D5\u05D5\u05D1 \u05DB\u05D3\u05D9 \u05E9\u05EA\u05D5\u05DB\u05DC\
  \ \u05DC\u05E2\u05D1\u05D5\u05D3 \u05D0\u05D9\u05EA\u05D5 \u05DE\u05E7\u05D5\u05DE\
  \u05D9\u05EA. \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E1\u05D5\u05E3 \u05DE\
  \u05D9\u05D3\u05E2, \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D6\u05DE\u05D9\u05E0\u05D5\
  \u05EA \u05E9\u05DC\u2026"
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
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
