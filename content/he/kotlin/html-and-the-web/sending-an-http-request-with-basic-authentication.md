---
date: 2024-01-20 18:02:18.923779-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.271034-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

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
