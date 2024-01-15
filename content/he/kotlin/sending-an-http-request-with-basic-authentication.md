---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Kotlin: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

##למה
מנסיון השימוש באפליקציות והתכנתות, כדאי לשרוד את משתמשי האפליקציות עם הרבה אפוסי LTC שמונים על חלוקת סיסמה בשירות האתר שלהם.כמו כן, מספק יתרונות כמו טביעת אצבעות, צידודיות, אבטחת אבטחת אתר וכו ' כדי למנוע חשיפת פרטים אישיים.

##איך ל
תכנות HTTP POST ב-Kotlin יכול להיות קל ופשוט עם אימות בסיס של כיתוב בסיסי. למשל, בשימוש בספרייה okhttp3 תוכל<br> להשתמש ב- `Request.Builder()` כדי לבנות את הבקשה ולכלול את השיטה `.header()` כדי להוסיף אימות בסיסי עם שם המשתמש והסיסמה. הנה דוגמאת קוד מלא עם אימות בסיסי:

```Kotlin
fun sendPostRequest() {
    val url = "https://example.com/api"
    val requestBody = "Some data to be posted"
    
    val credentials = Credentials.basic("username", "password")
    val request = Request.Builder()
        .url(url)
        .post(requestBody.toRequestBody())
        .header("Authorization", credentials)
        .build()
        
    val client = OkHttpClient()
    val response = client.newCall(request).execute()
    println(response.body()?.string())
}
```

יציאה לדוגמא הנ"ל תהיה: `"Some data to be posted"`, תעבור בהצלחה תוך A `200 OK`.

## צליל משנה

כעת, נכנסים לתמצית עמוקה יותר על פרוטוקול ה- HTTP ואימות בסיסי. האחריות העיקרית של אימות בסיסי היא למנוע גישה למידע סודי וגישה לפעולות ללא רשיות. כאשר משתמש מתחבר לאתר עם אימות בסיסי, הם יצטרכו לספק שם משתמש וסיסמה. הפרטים האלו ישמו לטבלת המשתמשים של האתר/אפליקציה כדי לוודא שהמשתמש יש מספיק רשיות ועד לשלב זה האתר יאשר את המשתמשות. 

##ראה גם
- [אימות בסיסי ב-Kotlin עם אופן התכנות](https://www.baeldung.com/kotlin-http-post-request)
- [HTTP ואימות ב