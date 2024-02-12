---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
aliases: - /he/c-sharp/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:18.572098-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי זה פשוט: מתקשרים עם שרת תוך סיפוק שם משתמש וסיסמה. המתכנתים עושים זאת כדי להבטיח שהדאטה שהם שולחים או מקבלים מוגנת ונגישה רק למשתמשים מורשים.

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
