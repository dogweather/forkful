---
date: 2024-01-20 18:00:40.402182-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05E7\u05D5\
  \u05D8\u05DC\u05D9\u05DF \u05D0\u05E0\u05D7\u05E0\u05D5 \u05D9\u05DB\u05D5\u05DC\
  \u05D9\u05DD \u05DC\u05E9\u05DC\u05D5\u05D7 \u05D1\u05E7\u05E9\u05D5\u05EA HTTP\
  \ \u05D1\u05E6\u05D5\u05E8\u05D4 \u05E7\u05DC\u05D4 \u05E2\u05DD \u05E1\u05E4\u05E8\
  \u05D9\u05D5\u05EA \u05DB\u05DE\u05D5 Ktor \u05D0\u05D5 OkHttp. \u05DB\u05D0\u05DF\
  \ \u05DE\u05EA\u05D5\u05D0\u05E8\u05EA \u05E9\u05D9\u05D8\u05D4 \u05E4\u05E9\u05D5\
  \u05D8\u05D4 \u05E2\u05DD Ktor."
lastmod: '2024-03-13T22:44:39.266517-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E7\u05D5\u05D8\u05DC\u05D9\u05DF \u05D0\u05E0\u05D7\u05E0\u05D5\
  \ \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\u05E9\u05DC\u05D5\u05D7 \u05D1\u05E7\
  \u05E9\u05D5\u05EA HTTP \u05D1\u05E6\u05D5\u05E8\u05D4 \u05E7\u05DC\u05D4 \u05E2\
  \u05DD \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5 Ktor \u05D0\u05D5\
  \ OkHttp."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

## איך לעשות:
בקוטלין אנחנו יכולים לשלוח בקשות HTTP בצורה קלה עם ספריות כמו Ktor או OkHttp. כאן מתוארת שיטה פשוטה עם Ktor.

```Kotlin
// ייבא את החבילה הנחוצה
import io.ktor.client.*
import io.ktor.client.request.*
import io.ktor.client.engine.cio.*

suspend fun main() {
    // יצירת לקוח HTTP
    val client = HttpClient(CIO)

    try {
        // קבלת תגובה מהשרת
        val response: String = client.get("http://example.com")
        println(response)
    } finally {
        // סגירת הלקוח
        client.close()
    }
}
```

כשתריצו את הקוד, התוצאה תהיה כזו:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## עיון נוסף:
1. **הקשר ההיסטורי**: בעבר השימוש ב-Java לשליחת בקשות HTTP היה יחסית מסורבל. התמיכה ב-HTTP השתפרה במשך השנים עם ספריות כמו Apache HttpClient, הוספת הממשק HttpURLConnection ב-Java, ובסוף עם כניסת ספריות מודרניות כמו OkHttp ו-Ktor בשפת קוטלין.
2. **אלטרנטיבות**: חוץ מ-Ktor, יש גם ספריות אחרות כמו Retrofit, Fuel, ו-OkHttp אשר מגלמות את אותו פונקציונליות עם דגשים וממשקים שונים.
3. **פרטי יישום**: ניתן להגדיר טיימאאוטים, headers, פרמטרים של קווארי ועוד בעת שליחת בקשות HTTP. מומלץ לבחון את מסמכי הממשק של הספרייה שברצונך להשתמש לפרטים נוספים.

## ראה גם:
- [מדריך Ktor הרשמי](https://ktor.io/docs/request.html)
- [מסמך OkHttp](https://square.github.io/okhttp/)
- [מדריך אינטרנט ל-APIs ב-Java](https://www.baeldung.com/java-9-http-client)
