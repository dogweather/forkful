---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:02:10.210888-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא שיטה לאבטחת גישה למשאבי אינטרנט. מתכנתים מבצעים זאת כדי להבטיח שרק משתמשים עם הרשאות נכונות יוכלו לגשת למידע או לפעולות מוגבלות.

## איך לעשות:
```Java
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class HttpClientBasicAuth {

    public static void main(String[] args) {
        try {
            URL url = new URL("http://example.com/api/resource");
            String auth = "username:password";
            String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            
            connection.setRequestMethod("GET");
            connection.setRequestProperty("Authorization", "Basic " + encodedAuth);
            
            int responseCode = connection.getResponseCode();
            System.out.println("Response Code: " + responseCode);

            if (responseCode == HttpURLConnection.HTTP_OK) {
                BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                String inputLine;
                StringBuilder response = new StringBuilder();

                while ((inputLine = in.readLine()) != null) {
                    response.append(inputLine);
                }
                in.close();

                System.out.println(response.toString());
            } else {
                System.out.println("GET request not worked");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
הפלט:
```
Response Code: 200
{json response from server}
```

## טבילה עמוקה
שליחת בקשות HTTP עם אימות בסיסי היא אחת מהשיטות הקדמיות ביותר באמצעות שם משתמש וסיסמא כדי לאמת גישות למערכות מרוחקות. זה פשוט אך לא הכי בטוח. אלטרנטיבות כמו OAuth או אמצעי אימות טוקן-בסיסיים מציעים שכבות בטיחות נוספות. עם זאת, ישנם מקרים שבהם בקשה פשוטה עם אימות בסיסי היא הפתרון המתאים ביותר, כמו שימוש בAPIs פנימיים שכבר מאובטחים על ידי רשתות ארגוניות. כשמשתמשים באימות בסיסי, חשוב להעביר בקשות אך ורק דרך HTTPS להגנה מפני האזנות לרשת.

## ראה גם
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [How to use Java HttpURLConnection to send a HTTP request](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Base64 Encoding and Decoding in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
- [Understanding HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)