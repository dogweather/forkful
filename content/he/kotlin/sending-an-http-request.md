---
title:                "שליחת בקשת http"
html_title:           "Kotlin: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

כמה פעמים התקבלתם עם הצורך לקבל מידע משרת אינטרנט או לשלוח מידע לשרת? נניח שיש לכם אפליקציה שנמצאת על הטלפון שלכם ורוצים לקבל את התוכן העדכני ביותר מהאתר המקורי. במקרים כאלה, כדאי להשתמש בפעולה שנקראת "שליחת דרישת HTTP". זהו סוג של בקשה שנשלחת מהמחשב או המכשיר שלכם לשרת אינטרנט. במאמר הזה, אני אסביר לכם כיצד להשתמש בקוד Kotlin כדי לשלוח בקשות HTTP ולקבל חזרה מידע מהשרת.

## מדוע

השליחה של בקשות HTTP נחשבת לאלטרנטיבה מעניינת יותר לשיטות הקיימות, כמו שימוש בפונקציות ספציפיות כדי להתחבר לשרת. הרבה מאפייני האינטרנט שאנו מכירים היום, כגון אתרי אתרי שיחות ואינטרנט, משתמשים בתקשורת HTTP. כך שלמדוק אז אתם יכולים ללכת מטה להערכת הקוד כדי לראות איך זה עושה.

## כיצד

כדי לשלוח בקשות ה-HTTP באמצעות קוד Kotlin, ניתן להשתמש בספריה שנקראת "kotlinx.serialization" כדי ליצור את הבקשה. הנה דוגמא לשליחת בקשה HTTP GET פשוטה ולקבלת התגובה בפורמט JSON:

```Kotlin
suspend fun makeGetRequest(url: String): String {
    val request = HttpClient().get<String>(url)
    return request
}
```

להלן דוגמא נוספת של שליחת בקשת POST עם נתונים מסוג JSON וקבלת תגובה גם היא בפורמט JSON:

```Kotlin
suspend fun makePostRequest(url: String, data: String): String {
    val request = HttpClient().post<String>(url) {
        this.body = data
        this.bodyContentType(ContentTypes.Application.Json)
    }
    return request
}
```

חשוב לציין כי בשני הדוגמאות האנחנו משתמשים בפונקציה "suspend" שמסמנת שהפונקציה מס