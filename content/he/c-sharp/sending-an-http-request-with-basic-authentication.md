---
title:                "C#: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

למה יש לכם צורך לשלוח בקשת HTTP עם אימות בסיסי? בפוסט זה נלמד על הטכניקות השונות שאפשר להשתמש בהן כדי לשלוח בקשת HTTP עם אימות בסיסי בשפת C#.

## איך לעשות זאת

בכדי לשלוח בקשת HTTP עם אימות בסיסי, נצטרך להשתמש במחלקת `HttpClient` ובמחלקת `AuthenticationHeaderValue`. נתחיל עם דוגמא פשוטה שתמחיש כיצד להגדיר את האימות בכתובת ה- `URL` של הבקשה:

```
using System;
using System.Net.Http;

namespace BasicAuthExample
{
    class Program
    {
        static async void Main(string[] args)
        {
            // יצירת מופע של HttpClient
            HttpClient client = new HttpClient();

            // הגדרת שם משתמש וסיסמה
            string username = "myusername";
            string password = "mypassword";

            // בניית האימות עם השם משתמש והסיסמה שהוגדרו
            string parameter = username + ":" + password;
            byte[] bytes = System.Text.Encoding.UTF8.GetBytes(parameter);
            string base64 = Convert.ToBase64String(bytes);
            string authentication = "Basic " + base64;

            // הגדרת כותרת האימות בבקשת ה-HTTP
            client.DefaultRequestHeaders.Add("Authorization", authentication);

            // שליחת בקשת HTTP GET עם אימות בסיסי
            HttpResponseMessage response = await client.GetAsync("https://www.example.com");

            // הדפסת קוד התגובה
            Console.WriteLine(response.StatusCode); // אם קיבלנו קוד 200 זה אומר שהתחברנו בהצלחה
        }
    }
}
```

## חקירה מעמיקה

אם ברצונכם לחקור עוד על השליחה של בקשת HTTP עם אימות בסיסי, כדאי לקרוא עוד על פרוטוקול האימות הבסיסי (Basic Authentication Protocol) ועל המחלקות `HttpClient` ו- `AuthenticationHeaderValue` במסמך הרשמי של C#.

## וכמו כן, באפשרותכם למצוא מידע נוסף על הטכניקות השונות של שליחת בקשות HTTP עם אימות בסיסי באתרים הבאים:

* [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
* [Stack Overflow](https://stackoverflow.com/questions/10315354/c-sharp-http-basic-authentication)
* [C# Corner](https://www.c-sharpcorner.com/uploadfile/puranindia/http-basic-authentication/)