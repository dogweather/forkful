---
date: 2024-01-20 17:59:28.860972-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D5 \u05DE\
  \u05D1\u05E7\u05E9\u05D9\u05DD \u05DE\u05E9\u05E8\u05EA \u05D1\u05E8\u05E9\u05EA\
  \ \u05DC\u05E9\u05DC\u05D5\u05D7 \u05D0\u05DC\u05D9\u05E0\u05D5 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05E4\u05E2\u05D5\u05DC \u05D1\u05DE\u05E1\
  \u05D5\u05D9\u05DD. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E1\u05D5\
  \u05E3 \u05DE\u05D9\u05D3\u05E2, \u05DC\u05E9\u05DC\u05D5\u05D7 \u05D8\u05E4\u05E1\
  \u05D9\u05DD, \u05DC\u05D0\u05DE\u05EA \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\
  \ \u05D5\u05E2\u05D5\u05D3,\u2026"
lastmod: 2024-02-19 22:04:58.567326
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D5 \u05DE\
  \u05D1\u05E7\u05E9\u05D9\u05DD \u05DE\u05E9\u05E8\u05EA \u05D1\u05E8\u05E9\u05EA\
  \ \u05DC\u05E9\u05DC\u05D5\u05D7 \u05D0\u05DC\u05D9\u05E0\u05D5 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05E4\u05E2\u05D5\u05DC \u05D1\u05DE\u05E1\
  \u05D5\u05D9\u05DD. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E1\u05D5\
  \u05E3 \u05DE\u05D9\u05D3\u05E2, \u05DC\u05E9\u05DC\u05D5\u05D7 \u05D8\u05E4\u05E1\
  \u05D9\u05DD, \u05DC\u05D0\u05DE\u05EA \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\
  \ \u05D5\u05E2\u05D5\u05D3,\u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא פעולה שבה אנו מבקשים משרת ברשת לשלוח אלינו נתונים או לפעול במסוים. תכניתנים עושים זאת כדי לאסוף מידע, לשלוח טפסים, לאמת משתמשים ועוד, כחלק מאפליקציות ובאתרי אינטרנט.

## איך לעשות:
```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

public class HttpExample
{
    private static readonly HttpClient client = new HttpClient();

    public static async Task Main(string[] args)
    {
        try
        {
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();

            Console.WriteLine(responseBody);
        }
        catch(HttpRequestException e)
        {
            Console.WriteLine("\nException Caught!");
            Console.WriteLine("Message :{0} ",e.Message);
        }
    }
}
```

פלט לדוגמה:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

כפי שאפשר לראות, בקוד הזה אנחנו שולחים בקשת GET ומדפיסים את התוכן.

## עיון נוסף:
בקשת HTTP היא חלק מהפרוטוקול HTTP, שנכנס לשימוש בשנת 1991. מאז, פותחו טכנולוגיות אחרות כמו WebSocket ו-GRPC, אך HTTP נשאר הדרך הפופולרית ביותר לתקשורת בין לקוח לשרת.

יש כמה דרכים לשלוח בקשות בC#, כמו `HttpClient`, `WebClient` (לא מומלץ מאז .NET Core) ו-`HttpWebRequest` (ישן יותר). `HttpClient` הוא הכלי האידיאלי בשל היכולת והיעילות שלו.

## כלי עזר נוספים:
- [מדריך ל`HttpClient` במיקרוסופט](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [תיעוד פרוטוקול HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
