---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי היא שיטה שבה מבצעים בקשה למשאב ומעבירים שם משתמש וסיסמה בצורה של מחרוזת מוצפנת. המתכנתים משתמשים בזה כדי להבטיח שרק משתמשים מאומתים יכולים לגשת למשאבים מסויימים.

## איך לעשות:

ראשית, פונקציה שמייצר שם משתמש וסיסמא מקודדים בסיסיים:

```C#
private static string CreateBasicAuthenticationHeaderValue(string username, string password)
{
    var byteArray = Encoding.ASCII.GetBytes($"{username}:{password}");
    return Convert.ToBase64String(byteArray);
}
```

בנוסף, פונקציה לשליחת בקשת ה-HTTP עם האימות:

```C#
private static async Task SendHttpRequestWithBasicAuthentication(string url, string username, string password)
{
    using (var client = new HttpClient())
    {
        client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", CreateBasicAuthenticationHeaderValue(username, password));
        var result = await client.GetStringAsync(url);
        Console.WriteLine(result);
    }
}
```

## צלילה עמוקה:

השיטה של HTTP בסיסי הוצגה לראשונה בהתקנה 1.0 של HTTP ונכתבה תוך כדי הנחה שהקשרי SSL / TLS לא מגנים תמיד על נתונים. ישנן חלופות אפשריות כמו OAuth או JWT שמעניקות אבטחה וריבוי תכליתים רחב יותר. תחת ההוצאה, הבקשה מוצפנת ב-'Basic' אחרי המרה של שם המשתמש והסיסמה ל-Base64.

## ראה גם:

[מאמרים נוספים](https://stackoverflow.com/questions/4015324/http-request-with-post)
[תיעוד המיקרוסופט](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)