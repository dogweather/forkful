---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# מה ולמה?
הורדת דף אינטרנט היא התהליך שבו מחשב קורא נתונים מדף אינטרנט קיים ושומר אותם מקומית. מתכנתים עושים את זה כדי לבצע אנליזה ועיבוד של מידע מהאינטרנט.

# איך מבצעים:
```C# 
using System.Net.Http;
using System.Threading.Tasks;

public class Program
{
    public static async Task Main(string[] args)
    {
        string url = "http://www.example.com";
        HttpClient client = new HttpClient();
        string result = await client.GetStringAsync(url);
        
        Console.WriteLine(result);
    }
}
```
תצאת דוגמאות אלו הייתה להיות קוד HTML של הדף www.example.com.

# שיעור מעמיק:
ההורדה של דף אינטרנט נעשתה לראשונה ב 1990 על ידי סיר טים ברנרס-לי. באופן אלטרנטיבי, אפשר לשרת את הדף על ידי שימוש ב-`WebClient` במקום `HttpClient`. `HttpClient` מועדף מכיוון שהוא מאפשר מניפולציה גבוהה מסיים מסיים ומכוד של הבקשה/תגובה.

# ראו גם: 
1. [איך להוריד דף אינטרנט ב-Python](https://realpython.com/python-requests/)
2. [התיעוד הרשמי של HttpClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)