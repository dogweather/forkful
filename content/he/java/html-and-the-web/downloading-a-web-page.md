---
date: 2024-01-20 17:44:58.833335-07:00
description: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\
  \u05D5 \u05D0\u05E0\u05D5 \u05E9\u05D5\u05DC\u05E4\u05D9\u05DD \u05D0\u05EA \u05D4\
  \u05EA\u05D5\u05DB\u05DF \u05E9\u05DC \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8 \u05DC\u05DE\u05D7\u05E9\u05D1 \u05E9\u05DC\u05E0\u05D5. \u05EA\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05DC\u05D1\u05E6\u05E2 \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D0\
  \u05D5 \u05DC\u05D0\u05D2\u05D5\u05E8 \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF\
  ."
lastmod: '2024-03-13T22:44:39.128784-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8 \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5\
  \ \u05D0\u05E0\u05D5 \u05E9\u05D5\u05DC\u05E4\u05D9\u05DD \u05D0\u05EA \u05D4\u05EA\
  \u05D5\u05DB\u05DF \u05E9\u05DC \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\
  \u05D8 \u05DC\u05DE\u05D7\u05E9\u05D1 \u05E9\u05DC\u05E0\u05D5."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

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
