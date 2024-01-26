---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:00:40.402182-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא התהליך שבו אפליקציה שולחת נתונים לשרת או מבקשת מידע ממנו. תכנתים עושים זאת כדי לאפשר תקשורת בין האפליקציה למשאבים מרוחקים, לדוגמה שליפת מידע מ-API או שליחת נתונים לעיבוד.

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
