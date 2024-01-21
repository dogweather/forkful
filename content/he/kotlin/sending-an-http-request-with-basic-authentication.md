---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:02:18.923779-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא תהליך שבו משתמש שולח שם משתמש וסיסמה בצורת קוד Base64 ב-header של בקשת HTTP. תוכניתנים עושים זאת כדי להבטיח את גישת המשתמש לרכיבי מערכת מוגנים.

## איך לעשות:
```Kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun sendGetRequestWithBasicAuth(url: String, username: String, password: String) {
    val urlConnection = URL(url).openConnection() as HttpURLConnection
    val credentials = "$username:$password"
    val basicAuth = "Basic ${Base64.getEncoder().encodeToString(credentials.toByteArray())}"

    urlConnection.requestMethod = "GET"
    urlConnection.setRequestProperty("Authorization", basicAuth)

    val responseCode = urlConnection.responseCode
    println("Response Code: $responseCode")
    urlConnection.inputStream.bufferedReader().use { it.lines().forEach { line -> println(line) } }
}

// דוגמא לשימוש:
val demoUrl = "http://example.com/api/data"
val username = "user"
val password = "password123"
sendGetRequestWithBasicAuth(demoUrl, username, password)
```
תוצאה:
```
Response Code: 200
{"message":"Data retrieved successfully"}
```

## עיון מעמיק:
אימות בסיסי ב-HTTP משמש כבר שנים רבות. בטרם הצפנת SSL ו-TLS הפך לשכיח, האימות היה חשוף ופגיע לחטיפת חיבורים. אלטרנטיבות מודרניות כוללות OAuth ו-API keys שמספקות שכבות אבטחה נוספות. כאשר שולחים בקשה עם אימות בסיסי ב-Kotlin, חשוב לשים לב לכך שסיסמאות אינן חושפות בקוד ולהשתמש באימות בטוח כמו HTTPS כדי למנוע האזנות סתר.

## ראה גם:
- המסמכים הרשמיים של Kotlin: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- המפרט של אימות בסיסי ב-HTTP: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- מדריך לקנה מידה מאובטח: [https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)