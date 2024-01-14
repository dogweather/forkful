---
title:                "Kotlin: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# למה
נשלחת בקשת HTTP עם אימות בסיסי? דוגמה לכך כוללת גישה לאתר אינטרנט או לשירות חיצוני שמצריך כניסה למשתמשים מורשים לפני שגישה תינתן לפרטים באמצעות בקשת HTTP.

# איך לעשות זאת
הנה דוגמה לקוד קוטלין עם הוספת אימות בסיסי בשאילתת HTTP:

```Kotlin
val url = "https://example.com/api/users"
val username = "username"
val password = "password"
val auth = "$username:$password"
val encodedAuth = Base64.getEncoder().encodeToString(auth.toByteArray())

val request = Request.Builder()
    .url(url)
    .addHeader("Authorization", "Basic $encodedAuth")
    .build()

val response = OkHttpClient().newCall(request).execute()

// בדיקת תגובה הצלחה
if (response.isSuccessful) {
    // המשך עם עיבוד התוצאה
    // כמו לאחזר את הנתונים מהתוכן של התגובה
    println("Response body: ${response.body?.string()}")
} else {
    // מקרה בו יש תגובה נכונה עם שגיאה
    // כמו להדפיס את קוד התגובה וההודעה
    println("Response code: ${response.code}")
    println("Response message: ${response.message}")
}
```

תוצאה דוגמתית:

```
Response body: {
    "id": 1234,
    "name": "John Doe",
    "email": "johndoe@example.com"
}
```

# טפסים עמוקים
כאשר אנחנו שולחים בקשת HTTP עם אימות בסיסי, אנו מעבירים פרמטרים שמאפיינים את זהות המשתמש והסיסמה באמצעות הכותרת "Authorization". כדי לבצע אימות בסיסי בקשת HTTP, אנו צריכים לקודד את זהות המשתמש והסיסמה בבסיס 64, כך שניתן יהיה להעביר אותם כמחרוזת בכותרת HTTP.

כדי לקודד את זהות המשתמש והסיסמה בבסיס 64 בקוד קוטלין, אנו משתמשים במחלקת Base64 ובפונקציית encodeToString להמרת הפרמטרים לתבנית המתאימה.

# ראה גם
- [פרסום מידע HTTP כולל אימות בסיסי](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [ספריית OKHttp לקוד קוטלין](https://square.github.io/okhttp