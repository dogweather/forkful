---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:44:19.535732-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

להוריד דף אינטרנט זה פשוט להשיג את התוכן של אתר מהרשת. תכנתים עושים את זה כדי לעבד נתונים, לבדוק אתרים, או לאחסן מידע מקוון לשימוש מקומי.

## איך לעשות:

בקוטלין, אתה יכול להוריד דף אינטרנט בקלות עם `URL.readText()`. הנה דוגמה:

```kotlin
import java.net.URL

fun downloadWebPage(pageUrl: String): String {
    return URL(pageUrl).readText()
}

fun main() {
    val content = downloadWebPage("https://example.com")
    println(content)
}
```

תוצאה (חלקית):

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## צלילה עמוקה:

בעבר, הייתם צריכים להשתמש ב`HttpURLConnection` או בספריות חיצוניות כמו Apache HttpClient להורדת תוכן מהאינטרנט. כיום, יש אפשרויות יותר נוחות כמו `kotlinx.coroutines` עם `async` לעבודה עם רשתות, או כלי חיצוניים כמו OkHttp או Retrofit לנתיני ה-API. רק זכרו, כשאתם מורידים דף אינטרנט, נתחו את תנאי השירות וה-robots.txt של האתר כדי לא להפר את הכללים.

## ראה גם:

- [kotlinx.coroutines](https://kotlinlang.org/docs/reference/coroutines-overview.html)
- [OkHttp](https://square.github.io/okhttp/)
- [Retrofit](https://square.github.io/retrofit/)
- [Robots exclusion standard](https://en.wikipedia.org/wiki/Robots_exclusion_standard)
