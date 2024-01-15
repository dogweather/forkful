---
title:                "שליחת בקשת http"
html_title:           "Java: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה 

למה לשלוח בקשת HTTP? כאשר מתכנתים מפתחים אפליקציות או אתרים, הם לעתים קרובות צריכים לשלוח ולקבל מידע משרתים חיצוניים. בקשת HTTP היא הדרך הנפוצה והנוחה ביותר לעשות זאת.

## כיצד לעשות זאת

כדי לשלוח בקשת HTTP ב-Java, נשתמש בממשק `HttpURLConnection` ובאובייקט `URL` כדי ליצור קשר עם השרת הרצוי. להלן דוגמא פשוטה של קוד ששולח בקשת GET לאתר גוגל ומדפיס את התוצאה בקונסולה:

```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

public class HTTPRequestExample {

    public static void main(String[] args) throws IOException {

        // יצירת אובייקט URL שמייצג אתר גוגל
        URL url = new URL("https://www.google.com");

        // יצירת אובייקט HttpURLConnection כדי ליצור קשר עם השרת
        HttpURLConnection con = (HttpURLConnection) url.openConnection();

        // קביעת שיטת הבקשה ל-GET
        con.setRequestMethod("GET");

        // קריאת התוכן של התגובה
        BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));

        String inputLine;
        StringBuffer content = new StringBuffer();

        while ((inputLine = in.readLine()) != null) {
            content.append(inputLine);
        }

        // הדפסת התוצאה בקונסולה
        System.out.println(content.toString());

        // סגירת החיבור
        in.close();
    }
}
```

בקוד זה, אנו משתמשים בשיטת `GET` כדי לשלוח את הבקשה. ישנן גם שיטות אחרות כמו `POST`, `PUT` ו־`DELETE` המשמשות לתקשורת עם השרת בצורות שונות. כאשר נצטרך לשלוח מידע נוסף כמו פרמטרים או גוף נתונים, נשתמש במחלקת `HttpURLConnection` כדי להגדיר ולשלוח את הפרמטרים המתאימים.

## העומק של שליחת בקשת HTTP

בכתבה זו, למדנו כיצד לשלוח בקשת HTTP ב-Java באמצעות הממשק `HttpURLConnection`. ניתן להשתמש בקוד הזה כדי לתקשר עם שרתים חיצוניים ולקבל מידע במגוון צורות כג