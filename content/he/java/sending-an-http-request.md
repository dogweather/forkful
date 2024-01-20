---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP היא דרך שבאמצעותה מחשב או אפליקציה "דוברת" עם שרת ברשת. מפתחים משתמשים בכך כדי לגשת למידע, לעבוד עם ממשקים של שירותים אינטרנטיים ולשלוט במערכות מרוחקות.

## איך עושים את זה:

Java 11 מציעה את חבילת `java.net.http` שמאפשרת לנו לקבל מידע מאתר אינטרנט באמצעות בקשת HTTP. הנה דוגמה:

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
              .uri(new URI("http://example.com"))
              .build();

        HttpResponse<String> response =
              client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());
    }
}
```

זה מראה את תגובת השרת מאתר example.com. 

## Deep Dive:

שליחת בקשות HTTP אינה בהכרח דבר חדש - Java 8 כבר הכילה את HttpUrlConnection, אך היישום היה קשה יותר. עם מהדורת Java 11, ביצעה Oracle שדרוג משמעותי בתוספתו של HttpClient.

בנוסף, ישנם שירותים דרישים כמו OkHttp וApache HttpClient שעושים את אותה העבודה, אך ללא דרישה לשדרוג לגרסה החדשה ביותר של Java. 

אפשר לשנות את פרטי הבקשה כדי להתאים לדרישות מסוימות - כולל שינוי של הכותרות, שליחת מידע בגוף הבקשה, וכו'.

## ראה גם:

* דוקומנטציה רשמית של [Oracle](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html) 
* מרכז למידה חינמי ל-[OkHttp](https://square.github.io/okhttp/)