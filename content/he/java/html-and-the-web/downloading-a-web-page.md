---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:44:58.833335-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

הורדת דף אינטרנט היא תהליך שבו אנו שולפים את התוכן של דף אינטרנט למחשב שלנו. תכניתנים עושים את זה כדי לעבד נתונים, לבצע בדיקות או לאגור את התוכן.

## איך לעשות:

במאמר זה, נשתמש ב-Java כדי להוריד דף אינטרנט. אנחנו מוסיפים תלות בספריית `java.net.*` כדי לבצע את המשימה.

```Java
import java.io.*;
import java.net.*;

public class WebPageDownloader {
    public static void main(String[] args) {
        String webPageUrl = "http://example.com";
        try {
            URL url = new URL(webPageUrl);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            
            BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String inputLine;
            StringBuilder content = new StringBuilder();
            
            while ((inputLine = in.readLine()) != null) {
                content.append(inputLine);
            }
            
            in.close();
            System.out.println("דף האינטרנט הורד בהצלחה");
        } catch (IOException e) {
            e.printStackTrace();
            System.out.println("הורדת דף האינטרנט נכשלה");
        }
    }
}
```

זה רק דוגמא סטטית פשוטה. לרוב, תרצו לאחסן את התוכן בקובץ או במאגר נתונים.

## צלילה לעומק:

היסטורית, הורדת דפי אינטרנט הייתה מורכבת יותר. בעבר היינו צריכים להתמודד עם הגדרות פרוקסי ותעבורה מוצפנת במאמצים רבים יותר. כיום, ספריות כמו Apache HttpClient ו-okhttp מקלות על התהליך. ישנם אלטרנטיבות ל-Java כמו cURL ב-PHP או Requests ב-Python.

בפירוט, כאשר אנו מורידים דף אינטרנט, אנו עושים בקשה מסוג GET לשרת. זה מחזיר לנו את תכני ה-HTML, שבהם יכולות להיות קישורים לתמונות, גיליונות סגנון (CSS), וסקריפטים. החיבור שיצרנו בדוגמא למעלה יכול גם להתמודד עם שגיאות שרת ולהחזיר את הקוד המתאים (לדוגמא, 404 לדף לא נמצא).

## ראו גם:

- [Java HttpURLConnection Documentation](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Apache HttpClient](https://hc.apache.org/httpcomponents-client-ga/)
- [okhttp](https://square.github.io/okhttp/)
- [JSoup for HTML Parsing in Java](https://jsoup.org/)
