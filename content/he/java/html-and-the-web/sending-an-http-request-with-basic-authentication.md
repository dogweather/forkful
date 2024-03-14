---
date: 2024-01-20 18:02:10.210888-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05E9\u05D9\u05D8\u05D4 \u05DC\u05D0\u05D1\u05D8\u05D7\u05EA \u05D2\u05D9\u05E9\
  \u05D4 \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\
  \u05D8. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7\
  \ \u05E9\u05E8\u05E7 \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05E2\u05DD \u05D4\
  \u05E8\u05E9\u05D0\u05D5\u05EA \u05E0\u05DB\u05D5\u05E0\u05D5\u05EA \u05D9\u05D5\
  \u05DB\u05DC\u05D5 \u05DC\u05D2\u05E9\u05EA \u05DC\u05DE\u05D9\u05D3\u05E2 \u05D0\
  \u05D5 \u05DC\u05E4\u05E2\u05D5\u05DC\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.130423-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05E9\u05D9\u05D8\u05D4 \u05DC\u05D0\u05D1\u05D8\u05D7\u05EA \u05D2\u05D9\u05E9\
  \u05D4 \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\
  \u05D8. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7\
  \ \u05E9\u05E8\u05E7 \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05E2\u05DD \u05D4\
  \u05E8\u05E9\u05D0\u05D5\u05EA \u05E0\u05DB\u05D5\u05E0\u05D5\u05EA \u05D9\u05D5\
  \u05DB\u05DC\u05D5 \u05DC\u05D2\u05E9\u05EA \u05DC\u05DE\u05D9\u05D3\u05E2 \u05D0\
  \u05D5 \u05DC\u05E4\u05E2\u05D5\u05DC\u05D5\u05EA\u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
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
