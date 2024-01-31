---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T17:59:28.860972-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"

category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/sending-an-http-request.md"
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
