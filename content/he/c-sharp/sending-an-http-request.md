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

## מה ולמה?

שליחת בקשת HTTP היא פעולה שמאפשרת לתוכנית לבקש מידע משרת אינטרנט אחר. תכנית יכולה לשלוח בקשת HTTP למגוון של מקורות, כולל אתרי אינטרנט ושירותי תוכן. תוכנית יכולה גם לבצע פעולות נוספות, כמו להוסיף, לערוך או למחוק מידע משרת באמצעות בקשת HTTP.

מדוע מתכנתים עושים זאת? בקשת HTTP היא כלי שימושי לקבלת נתונים וליצירת יישומים אינטרנטיים מתקדמים. על ידי שליחת בקשת HTTP, מתכנתים יכולים לקבל גישה למגוון רחב של מידע וליצור יישומים אינטרנטיים מתקדמים.

## איך לעשות זאת:

כדי לשלוח בקשת HTTP ב-C#, ניתן להשתמש בפעולת "WebRequest" ו-"WebResponse". לדוגמה, לשלוח בקשת GET לאתר מסוים:

```
HttpWebRequest request = (HttpWebRequest)WebRequest.Create("http://example.com");
HttpWebResponse response = (HttpWebResponse)request.GetResponse();
Console.WriteLine("Response Status Code: " + response.StatusCode); // מדפיס קוד סטטוס התגובה
Console.WriteLine("Response Status Description: " + response.StatusDescription); // מדפיס תיאור קוד סטטוס התגובה
```

כדי לשלוח בקשת POST ולקבל נתונים מפורמט JSON, ניתן להשתמש בפעולת "WebClient" ו-"UploadString":

```
string url = "http://example.com/api";
string postData = "{\"username\": \"John\", \"password\": \"1234\"}";
WebClient client = new WebClient();
client.Headers[HttpRequestHeader.ContentType] = "application/json";
string result = client.UploadString(url, "POST", postData);
```

תוצאה מודפסת: {"status": "success", "message": "User John logged in"}

## חפירה עמוקה:

בעבר, לשלוח בקשת HTTP הייתה נדרשת בעיקר לגישה לאתרים אינטרנט ולגישה למטא-נתונים ופתרונות רשת אחרים. אבל עם התפתחות האינטרנט והפיתוח הטכנולוגי, שליחת בקשת HTTP יכולה להיות חלק מיישומי רשת מתקדמים בתחומים רבים כגון מכשירים חכמים ואינטרנט הדברים.

אלטרנטיבות לשליחת בקשת HTTP כוללות את תוכניות ה-API ושימוש בפרוטוקולים אחרים כמו WebSocket ו-SPDY.

כדי לממש שליחת בקשת HTTP ב-C#, יש להשתמש בפעולות "WebRequest" ו-"WebResponse" או "WebClient". ישנם גם כלים נוספים כמו ממשקי תכנות כמו RestSharp ו-HttpClient המקלים על שליחת בקשת HTTP ועיבוד נתונים מהתגובה.

## ראה גם:

למידע נוסף על שליחת בקשת HTTP ב-C#, ניתן להתייעץ עם המסמכים הרשמיים של מיקרוסופט. כמו כן, ניתן למצוא מידע מפורט על פקודות שליחת בקשת HTTP ופעולות נוספות באתרים כמו W3C ו-MDN.