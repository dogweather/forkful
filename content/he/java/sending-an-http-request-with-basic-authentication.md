---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Java: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה
Sending an HTTP request with basic authentication allows the user to securely access protected web resources by providing a username and password. This is commonly used in applications that require user authentication.

## איך לבצע
מתוך דאטה ריספונס יוצרים משתמש פסטאיול 
```Java
Authenticator.setPasswordAuthentication(new URL("https://www.example.com"), 
                                        username, 
                                        password.toCharArray());
```
כדי לשלוח את הבקשה המאובטחת, ניתן להשתמש במתודת `setRequestProperty()` כדי להוסיף את פרמטר האימות לכותרת הבקשה:
```Java
connection.setRequestProperty("Authorization", "Basic " + basicAuth);
```
לאחר מכן, ניתן לשלוח את הבקשה בצורה רגילה עם `HttpURLConnection`:
```Java
HttpURLConnection connection = (HttpURLConnection) new URL(url).openConnection();
connection.setRequestMethod("GET");
```
להלן דוגמא מלאה של קוד לשליחת בקשה HTTP מתוך אפליקציית ג'אווה:
```Java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.Authenticator;
 
public class HttpBasicAuthenticationExample {
 
    public static void main(String[] args) {
 
        String username = "example_user";
        String password = "example_password";
        String url = "https://www.example.com";
 
        Authenticator.setPasswordAuthentication(new URL(url), username, password.toCharArray());
 
        HttpURLConnection connection = null;
        BufferedReader reader = null;
 
        try {
            //הוספת הפרמטר של האימות
            String basicAuth = "Basic " + javax.xml.bind.DatatypeConverter.printBase64Binary((username + ":" + password).getBytes());
 
            connection = (HttpURLConnection) new URL(url).openConnection();
            connection.setRequestMethod("GET");
            connection.setRequestProperty("Authorization", basicAuth);
 
            //קריאת התוכן מהגוף של התגובה
            reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String line;
            StringBuffer content = new StringBuffer();
            while((line = reader.readLine()) != null) {
                content.append(line);
            }
 
            System.out.println(content.toString());
 
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if(connection != null) {
                connection.disconnect();
            }
            if(reader != null) {
                try {
                    reader.close();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }
 
}
```

## צילום עמוק
כאשר אנו שולחים בקשת HTTP עם אימות בסיסי, הפרמטר של האימות מתווסף לכותרת הבקשה בצורת מחרוזת מקודדת בבייס 64. מימוש נוסף של מחלקת `Authenticator` יכול להשתמש באמצעות הפעלת `setDefault()` כדי לערוך את האימות הבסיסי לשימוש כיתוב בדיקה ואבחון בקשות HTTP. מדי פעם, עלול לכבות דפדפן של תוך HTML בלתי מוסכם מולא מנסה לכולל גוף של מידע מאחורי