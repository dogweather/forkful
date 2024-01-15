---
title:                "שליחת בקשת http"
html_title:           "C#: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

ככל שהעולם המקוון מתפתח, תקשורת בין אתרים היא כל כך נפוצה כי היא כמעט שאי אפשר לעבור עליה. אנשים משתמשים בתקשורת כדי לגשת למידע מרחוק, לשלוח דואר אלקטרוני או לפעול ברשתות חברתיות. אחת הדרכים הנפוצות לבצע תקשורת בין אתרים היא באמצעות בקשות HTTP, ובמאמר זה נלמד כיצד לשלוח בקשה זו בשפת סי שארפ.

## איך לעשות זאת

תחילה, נצטרך להתקין את חבילת התוכנה System.Net.Http באמצעות פקודת NuGet כדי להשתמש בפעולות של HTTP. לאחר מכן, נוכל ליצור את הבקשה בקוד הבא:

```C#
using System.Net.Http;

public async Task MakeHTTPRequest()
{
    HttpClient client = new HttpClient();
    HttpResponseMessage response = await client.GetAsync("https://www.example.com");
    string result = await response.Content.ReadAsStringAsync();
    Console.WriteLine(result);
}
```

בקוד זה, אנו משתמשים במחלקת HttpClient כדי ליצור את הבקשה לכתובת URL המבוקשת. ניתן להשתמש בעוד פעולות כגון PostAsync כדי לשלוח גם נתונים בגוף הבקשה. לאחר מכן, אנו משתמשים במחלקת HttpResponseMessage כדי לקבל את התגובה מהשרת, ונמיר את תוכן התגובה למחרוזת באמצעות פעולת ReadAsString. לבסוף, אנו מדפיסים את התוצאה לצורך צפייה בתוכן הקבצה שקיבלנו מהשרת.

## טיול מעמיק

כאשר מתחילים לעבוד עם בקשות HTTP, חשוב להיות מודעים למונחים והעקרונות הבסיסיים של התקשורת בין אתרים. נהוג לציין שהדבר הראשון שמשתמשים צריכים לעשות הוא לבדוק את הגרסה של פרוטוקול ה-HTTP שהשרת מכיל, ולהתאים את בקשת ה-HTTP המאופסת לתואמתו. כמו כן, חשוב לבדוק צורת הת