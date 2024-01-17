---
title:                "הורדת עמוד אינטרנט"
html_title:           "Kotlin: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# מה ולמה?
להוריד עמוד אינטרנט הוא פעולה שמאפשרת למשתמשים לגשת למידע ותוכן מאתרים באינטרנט. תוכנתנים משתמשים בפעולה זו כדי ליצור אפליקציות ומערכות שמאפשרות גישה למידע ותוכן דינמי מהאינטרנט.

# איך לעשות?
תוכנתנים יכולים להוריד עמוד אינטרנט בקלות באמצעות ליבות מוכנות כמו Kotlinx.IO ו-OkHttp. הנה דוגמאות להורדת עמוד אינטרנט באמצעות Kotlin:

```Kotlin
// להוריד עמוד אינטרנט עם Kotlinx.IO
val response = URL("https://www.example.com").readText()
println(response)

// להוריד עמוד אינטרנט עם OkHttp
val client = OkHttpClient()
val request = Request.Builder()
    .url("https://www.example.com")
    .build()
val response = client.newCall(request).execute()
println(response.body?.string()) 
```

פלט לקוד הנ"ל יהיה תוכן העמוד שהורד.

# נחישות
לאחרונה, פתחו תוכנתנים מספר כלים עבור הורדת עמודי אינטרנט, כגון Scrapinghub ו-Selenium. אלה כלים מאפשרים למשתמשים לייצר סקריפטים שמנוהלים על ידי הכלים ומייצרים דטה על יידול של עמודי אינטרנט.

עוד אפשרות היא שליחת בקשות HTTP ישירות לשרתים של האתר, כאשר התגובה המלאה של האתר מתקבלת בתוך התכנית.

# ראו גם
- אתר רשמי של Kotlin: https://kotlinlang.org/
- דוגמאות להורדת עמודי אינטרנט באמצעות Kotlin: https://github.com/Kotlin/kotlinx-io/blob/master/README.md#usage
- אתר רשמי של OkHttp: https://square.github.io/okhttp/
- כלי Scrapinghub: https://scrapinghub.com/
- כלי Selenium: https://www.selenium.dev/