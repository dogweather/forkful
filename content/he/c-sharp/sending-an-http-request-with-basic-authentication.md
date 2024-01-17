---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C#: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

מה זה ולמה?

שלוש כותרות מקצועיות שנראות קשוחות, אבל בפועל שלוש משפטים פשוטים שמסבירים מהי השליחה של בקשת HTTP עם אימות בסיסי ולמה מתכנתים עושים את זה.

## מה ולמה?

השליחה של בקשת HTTP עם אימות בסיסי היא תהליך בו מתכנתים שולחים בקשה לשרת עם מידע אישי להתחברות, כדי לאמת את המשתמש ולאפשר גישה למידע מוגן. המתכנתים עושים את זה כדי להבטיח שרק משתמשים מורשים יוכלו לקבל גישה למידע רגיש.

## איך לעשות:

מתחת לכותרת זו נמצאים דוגמאות קוד ופלט דוגמא, המציגים איך לשלוח בקשת HTTP עם אימות בסיסי בקוד C#.

```C#
// ייבוא המודולים הנחוצים:
using System;
using System.Net;
using System.IO;

// פונקציית עזר לפעולות HTTP:
public static string SendHttpRequest(string url, string username, string password)
{
    // יצירת אובייקט WebRequest:
    WebRequest request = WebRequest.Create(url);

    // הגדרת אימון בסיסי:
    string credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes(username + ":" + password));

    // הוספת הכותרת "Authorization" עם האימון הבסיסי לבקשה:
    request.Headers.Add("Authorization", "Basic " + credentials);

    // שליחת הבקשה:
    WebResponse response = request.GetResponse();

    // קריאה של התוכן המרוכז בתגובה:
    Stream dataStream = response.GetResponseStream();
    StreamReader reader = new StreamReader(dataStream);
    string responseFromServer = reader.ReadToEnd();

    // סגירת הירידות:
    reader.Close();
    response.Close();

    // החזרת התגובה:
    return responseFromServer;
}

// קריאה לפונקצייה עם כתובת ה-URL, שם משתמש וסיסמה:
string response = SendHttpRequest("https://www.example.com/api", "username", "password");

// הדפסת התגובה:
Console.WriteLine(response);
```

## חקירה מעמיקה:

בחלק זה נמצאים מידע נוסף על תהליך שליחת בקשת HTTP עם אימות בסיסי, כולל היסטוריה, אלטרנטיבות ופרטים טכניים נוספים.

### היסטוריה:

תהליך אימות בסיסי של בקשת HTTP נוצר כחלק מתקן האינטרנט RFC 2617 ונמצא בשימוש נרחב ביישומים רבים. מטרתו העיקרית היא לאפשר גישה מאובטחת לרכיבים אינטראקטיביים באינטרנט.

### אלטרנטיבות:

אלטרנטיבות לאימות בסיסי כוללות שימוש באימות מלאכותי (API key) או באימות OAuth. כל אלטרנטיבה מתאימה למטרות שונות ויישום המומלץ יכול להיות שונה תלוי ברכיב האינטראקטיבי עצמו.

### פרטים טכניים:

כדי לשלוח בקשת HTTP עם אימות בסיסי, משתמשים יכולים להשתמש בheader ה- "Authorization" ולכלול בו את האימון הבסיסי. בנוסף, הישג לתוכן מוגן יכול להיות גם דרך בקשת POST, עם