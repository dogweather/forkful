---
title:                "C#: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# למה?

HTTP בקשה מתקבלת בכמעט כל יישום תוכנה. היא מאפשרת לשרת לקבל בקשות מצד לקוחות ולשלוח חזרה תגובות. בקשת HTTP יכולה להיות מאוד שימושית כאשר מפתחים יישומי אינטרנט ותוכנות, ולכן חשוב להבין איך לשלוח בקשה כדי לשלוט בתגובות המתקבלות מהשרת. 

# איך לעשות זאת?

בכדי לשלוח בקשת HTTP ב-C#, יש לעקוב בכמה צעדים פשוטים:

1. הגדרת מופע של "HttpClient" עבור התקשורת עם השרת.
2. יצירת בקשה חדשה עם הפרמטרים המתאימים.
3. שליחת הבקשה לשרת ובדיקת התגובה.

ניתן לראות דוגמאות לקוד בהמשך, כדי שתוכלו להבין בצורה טובה יותר איך לממש את השלבים הנ"ל.

```C#
// הגדרת מופע של HttpClient
using System.Net.Http;

var client = new HttpClient();

// יצירת בקשה חדשה עם URL ומתאם
var url = "https://www.example.com";
var httpResponse = await client.GetAsync(url);

// בדיקת התגובה והצגת הקוד שלה
if (httpResponse.IsSuccessStatusCode)
{
    Console.WriteLine("הבקשה נשלחה בהצלחה!");
}
else
{
    Console.WriteLine($"קוד תגובת הבקשה: {httpResponse.StatusCode}");
}
```

תוצאת הריצה של הקוד הנ"ל יציג "הבקשה נשלחה בהצלחה!" אם הבקשה נשלחה בהצלחה, או קוד שגיאה אם משהו השתבש.

# המעמקים

כעת שינויים קטנים בקוד מבחינת הגדרת המופע ושליחת הבקשה יכולים להיות מאוד משמעותיים בהקשר של תגובות מהשרת. ניתן להוסיף כפתורי ביטול מתאימים באמצעות "CancelToken" וכפתור שליחת נתונים באמצעות "FormContent". כמו כן, ניתן לשלוח בקשת POST במקום בקשת GET כדי לשלוט בהתנהגות של