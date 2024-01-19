---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי הן דרך של קליינט לשלוח בקשה לשרת עם שם משתמש וסיסמה כאימות. מתכנתים משתמשים בכך כדי לאבטח את הנתונים ולצמצם את סיכון פגיעה בפרטיות.

## איך לעשות:
ב־Kotlin, אנחנו יכולים להשתמש בספריית Ktor כדי לשלוח בקשת HTTP עם אימות בסיסי:
```kotlin
val client = HttpClient() {
    install(Auth) {
        basic {
            username = "myUsername"
            password = "myPassword"
        }
    }
}

val request: HttpRequestBuilder.() -> Unit = {
    url(URL("https://example.com"))
}

val response: HttpResponse = client.get(request)
println(response.status)
```

## שיפוז מעמיק
במסגרת ההיסטוריה של פיתוח תוכנה, שליחת בקשת HTTP עם אימות בסיסי היא אחת הדרכים הראשונות לאבטחת שאילתות שלך אל שרת. דרך אלטרנטיבית לאימות בסיסי היא אימות באמצעות מחרוזת אקראית, כאשר אתה שולח מחרוזת מיוחדת בכותרת של הבקשה. נערך זה במקום או בנוסף לשם משתמש וסיסמה. במהלך השימוש באימות בסיסי, שם המשתמש והסיסמה משולחים בצורה לא מוצפנת, כך שזה לא המהלך הבטוח ביותר. 

## ראו גם
* [מסמך ה-API של Ktor](https://ktor.io/clients/http-client/features/auth.html)
* [מדריך אימות בסיסי מ- Mozilla](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
* [הדרכת אימות בסיסי מ- W3C](https://www.w3.org/Protocols/HTTP/1.0/spec.html#BasicAA)