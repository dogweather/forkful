---
date: 2024-01-20 18:02:18.572098-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D6\u05D4 \u05E4\
  \u05E9\u05D5\u05D8: \u05DE\u05EA\u05E7\u05E9\u05E8\u05D9\u05DD \u05E2\u05DD \u05E9\
  \u05E8\u05EA \u05EA\u05D5\u05DA \u05E1\u05D9\u05E4\u05D5\u05E7 \u05E9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9 \u05D5\u05E1\u05D9\u05E1\u05DE\u05D4. \u05D4\u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05E9\u05D4\u05D3\u05D0\
  \u05D8\u05D4 \u05E9\u05D4\u05DD \u05E9\u05D5\u05DC\u05D7\u05D9\u05DD \u05D0\u05D5\
  \ \u05DE\u05E7\u05D1\u05DC\u05D9\u05DD \u05DE\u05D5\u05D2\u05E0\u05EA \u05D5\u05E0\
  \u05D2\u05D9\u05E9\u05D4\u2026"
lastmod: '2024-03-13T22:44:39.342433-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D6\u05D4 \u05E4\
  \u05E9\u05D5\u05D8: \u05DE\u05EA\u05E7\u05E9\u05E8\u05D9\u05DD \u05E2\u05DD \u05E9\
  \u05E8\u05EA \u05EA\u05D5\u05DA \u05E1\u05D9\u05E4\u05D5\u05E7 \u05E9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9 \u05D5\u05E1\u05D9\u05E1\u05DE\u05D4."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

## איך לעשות זאת:
ב-C# אפשר לשלוח בקשת HTTP עם אימות בסיסי באמצעות כמה קווים של קוד:

```C#
using System;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        var url = "http://your-api-url.com";
        var username = "your-username";
        var password = "your-password";

        using var client = new HttpClient();
        var byteArray = Encoding.ASCII.GetBytes($"{username}:{password}");
        client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));

        HttpResponseMessage response = await client.GetAsync(url);
        if(response.IsSuccessStatusCode)
        {
            Console.WriteLine("Success:");
            Console.WriteLine(await response.Content.ReadAsStringAsync());
        }
        else
        {
            Console.WriteLine("Error:");
            Console.WriteLine(response.StatusCode);
        }
    }
}
```

נתון: קיבלתם תגובה מהשרת שמייצגת את הפעולה שביצעתם בהצלחה או כישלון.

## קצת יותר לעומק:
אימות בסיסי ב-HTTP נכנס לשימוש בראשית האינטרנט כדרך פשוטה להזדהות. זה לא הכי בטוח כי הסיסמאות שולחים ב-Base64, פורמט שקל מאוד לפענח. עם זאת, הוא עדיין נמצא בשימוש עבור שירותים פנימיים או כשיש שכבה נוספת של אבטחה כמו HTTPS.

יש חלופות בטוחות יותר כמו OAuth 2.0 וJWT (JSON Web Tokens) שמאפשרים אימות עם טוקנים. אלה מעניקים הרשאות לאורך זמן מוגבל ופחות פגיעות למתקפות.

ביצוע של בקשת HTTP עם אימות בסיסי ב-C# ישיר וקל עם החבילות של `HttpClient`. ועדיין, תמיד כדאי לשקול אם הבטחת המידע שלכם חשובה דיו שתשתמשו בשיטה בטוחה יותר.

## ראו גם:
- [RFC7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Microsoft Docs - HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [OWASP - Authentication Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)

נ.ב.: נושאי אבטחה משתנים מהר, תחזיקו טוב את האינפורמציה שלכם עדכנית.
