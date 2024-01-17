---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Kotlin: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי היא פעולה שנעשית על ידי מתכנתים כדי לאמת את זהותם ולקבל גישה למידע מאתרי אינטרנט שונים. תהליך זה נותן למתכנתים גישה מאובטחת למידע באתר רק על ידי זיהוי ואימות של המשתמש.

כיצד לבצע?

קודים ופלט מדוגמא ניתנים להבנה על ידי קידום השורות בקודים פקודה-כאן. טכניקות לחישוב של ריצודות ותוכלי להשתמש במאפיינים מובחנים ב-building (java.util.scanner) בקוד java.

Kotlin /**

```Kotlin
fun sendHttpRequestWithAuthentication() {
    val url = URL("http://example.com") // אתר שליחת בקשה
    val connection = url.openConnection() as HttpsURLConnection // חיבור HTTP
    connection.setRequestMethod("GET") // HTTP GET בקשה עם אימות בסיסי
    connection.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString("username:password".toByteArray())) // הוספת תוכן אימות בין תוכן לתוכן של אימות בסיסי כדי לאמת את המשתמש
    val responseCode = connection.getResponseCode() // קוד תגובה מהמרחק
    println("Response Code : $responseCode") // הדפסת הקוד של התגובה
    val bufferedReader = BufferedReader(InputStreamReader(connection.getInputStream())) // קריאת תגובת האימות
    var inputLine: String?
    val response = StringBuffer()
    while ((inputLine = bufferedReader.readLine()) != null) {
        response.append(inputLine)
    }
    bufferedReader.close()
    println("Response: ${response.toString()}") // הדפסת תגובת האימות
}�```


עומק נכנס:
לפני הצפיפות של תוכניות הגנה, אימות בסיסי היה נהוג כדרך קלה ופשוטה לאמת את זהות המשתמש. אם רוצים רמה נוספת של אבטחה, ישנם אלטרנטיבות כמו OAuth או OpenID שמאפשרות אימות עם מכשירי טוקנים ייחודיים. בנוסף, היכולת לשלוח בקשות HTTP עם אימות בסיסי מאפשרת למתכנתים לקבל גישה למידע מאתרים שונים ולאמת את זהות המשתמש באופן בטוח ומאובטח.

ראו גם:
למידע נוסף על אימות HTTP בסיסי, ניתן להשתמש במקורות הקשורים הבאים:
- [טכניקות אימות כבדות](https://www.tutorialspoint.com/http/http_authentication.htm)
- [מדריך לשליחת בקשות HTTP בסיסיות בפייתון](https://www.pythonforbeginners.com/requests/using-requests-in-python-basic-authentication/)
- [התיעוד של Kotlin על פעולות מערכת HTTP](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.-text/base64-encoding/index.html)