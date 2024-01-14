---
title:                "Kotlin: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

HTTP דרך העברת מידע ברשת והיא חלק בלתי נפרד מתהליך התקשורת בין כתובת האתר שבו אתם נמצאים לבין שרת האינטרנט שבו מארח האתר הנ"ל. כאשר אנחנו מבקשים משהו כגון דף אינטרנט, בכל רגע נתון אנחנו בעצם שולחים ומקבלים דוא"א התקשורת של HTTP.

## כיצד לעשות זאת

תהליך שליחת בקשת HTTP נעשה בשימוש במנגנון עז - הפונקציה print() של לוגיקת הקוד שלנו. לדוגמא הנה קטעי קוד בכתובת האתר שלנו המראים איך ניתן לשלוח ולקבל דוא"א HTTP עם לוגיקת הקוד של Kotlin:

```Kotlin
import java.net.HttpURLConnection
import java.net.URL

fun sendRequest(urlString: String) {
    val url = URL(urlString)
    val connection = url.openConnection() as HttpURLConnection
    connection.requestMethod = "GET"
    connection.connect()

    val responseCode = connection.responseCode
    println("Response Code: $responseCode")

    val responseMessage = connection.responseMessage
    println("Response Message: $responseMessage")
}
```

כאן, אנו משתמשים בפונקציה URL של לוגיקת הקוד של Kotlin כדי ליצור קישור לכתובת האתר הרצוי ואנו מגדירים את השיטה שבה נשלח את הבקשה, במקרה זה - "GET" (הבקשה לקבל מידע מהשרת). לאחר מכן, אנחנו מחברים לפני השילוב המעודכן של לוגיקת הקוד של Kotlin וקישור HTTP עם הפעולה connect() כדי לשלוח את הבקשה. לבסוף, אנחנו מדפיסים את קוד התגובה וההודעה שמתקבלים מהשרת.

כשנריץ את קוד זה, נקבל את הפלט הבא:

```
Response Code: 200
Response Message: OK
```

זה אומר שהבקשה שלנו התקבלה ומענה השרת הוא "OK" (כלומר הבקשה נמצאת בסדר).

כדי לשלוח בקשות HTTP מרחוק יום אחד אנחנו נוכל להשתמש בדוגמאות כמו GET, POST, PUT או DELETE כדי לפנות לשרת ולבקש מה שמעניין