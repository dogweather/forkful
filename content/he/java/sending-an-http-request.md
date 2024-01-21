---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:00:20.777171-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא דרך לתקשר עם שרתים באינטרנט - אתה שואל משהו, הם עונים. תכניתנים שולחים בקשות כאלה כדי לקבל נתונים, לשלוח מידע, או לבצע פעולות מרחוק.

## איך לעשות:
ב-Java, שליחת בקשת HTTP היא די פשוטה. חלק מהדוגמאות ישתמשו בממשק `HttpClient`, המבוסס על יכולות שהוצגו ב-Java 11:

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpExample {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("https://api.example.com/data"))
                .build();
        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
                .thenApply(HttpResponse::body)
                .thenAccept(System.out::println)
                .join();
    }
}
```

אם הכל עובד כמו שצריך, יודפס תוכן הבקשה שקיבלת.

## עיון מעמיק:
במשך שנים, שליחת בקשות HTTP ב-Java היתה קשה יותר. תכניתנים נאלצו להשתמש ב-`HttpURLConnection` או ספריות חיצוניות כמו Apache HttpClient. מ-Java 11 והלאה, הממשק `HttpClient` מספק דרך ישירה ומודרנית יותר. הוא כולל תמיכה ב-HTTP/2, שליחת בקשות אסינכרוניות ועוד.

אלטרנטיבות ל-`HttpClient` כוללות ספריות פופולריות כמו OkHttp וRetrofit, שמספקות יכולות נוספות וחוויית משתמש נעימה יותר לאנשים שעובדים הרבה עם REST APIs.

## ראה גם:
- [Documentation for the `HttpClient`](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html) - תיעוד רשמי מאורקל.
- [OkHttp](https://square.github.io/okhttp/) - דף הבית של OkHttp להורדות ותיעוד.
- [Retrofit](https://square.github.io/retrofit/) - דף הבית של Retrofit להורדות ותיעוד.