---
date: 2024-01-20 17:44:19.535732-07:00
description: "\u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8 \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D4\u05E9\
  \u05D9\u05D2 \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05E9\u05DC \u05D0\u05EA\
  \u05E8 \u05DE\u05D4\u05E8\u05E9\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\
  \u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D1\u05D3\u05D5\u05E7\
  \ \u05D0\u05EA\u05E8\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D0\u05D7\u05E1\u05DF \u05DE\
  \u05D9\u05D3\u05E2 \u05DE\u05E7\u05D5\u05D5\u05DF \u05DC\u05E9\u05D9\u05DE\u05D5\
  \u05E9 \u05DE\u05E7\u05D5\u05DE\u05D9."
lastmod: 2024-02-19 22:04:58.495902
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D4\u05E9\u05D9\
  \u05D2 \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05E9\u05DC \u05D0\u05EA\u05E8\
  \ \u05DE\u05D4\u05E8\u05E9\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\
  \u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\
  \u05EA\u05E8\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D0\u05D7\u05E1\u05DF \u05DE\u05D9\
  \u05D3\u05E2 \u05DE\u05E7\u05D5\u05D5\u05DF \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9\
  \ \u05DE\u05E7\u05D5\u05DE\u05D9."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
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
