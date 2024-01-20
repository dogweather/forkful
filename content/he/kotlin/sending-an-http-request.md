---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא תהליך בו המחשב שלך מבקש מידע משרת ווב. מתכנתים משתמשים בכך לאפשר התקשרות בין אפליקציות ושרתים על מנת לקבל, לשנות או למחוק מידע.

## איך לבצע:
```Kotlin
import kotlinx.coroutines.*
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun fetchFromServer(): String {
   val client = HttpClient()
   val response: String
   try{
       response = client.get("http://example.com")
   } finally{
       client.close()
   }
   return response
}

fun main() = runBlocking{
   println(fetchFromServer())
}
```
כאשר תריצו את הקוד, תראו בפלט את הדף הראשי של `example.com`.

## צולעים עמוק:
נושא של שליחת בקשת HTTP התפתח תחת מסגרת התקן של http, שדרך אינטרנט מאפשרת שרתים לשלוח מסמכים למחשבים. חלופות לשליחת בקשת HTTP משתמשת בפרוטוקולים אחרים, כמו FTP או gRPC. מידע אודות שליחת בקשת HTTP ב-Kotlin מתבצעת דרך HttpClient, שהוא ספריית גנריקית שמאפשרת שיח עם שרתים משונים.

## ראה גם:
למידע נוסף על שליחת בקשות HTTP אתם מוזמנים לבקר במשאבים הבאים:
- דוקומנטציה הרשמית של ktor: https://ktor.io/docs/http-client.html 
- הקוד המקורי של HttpClient: https://github.com/ktorio/ktor 
- מדריך מקיף לתקן HTTP: https://www.w3.org/Protocols/
   
כאמן שלחנו, במאמר זה סיפקנו סקירה מהירה על שליחת בקשות HTTP ב-Kotlin. תרגול מושך הוא הדרך הטובה ביותר להתמקד באיך לשלוח בקשות HTTP, אז המשך למקד קוד הקונה.