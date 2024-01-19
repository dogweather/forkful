---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה זה & למה?
שליחת בקשת HTTP עם אוטנטיקציה בסיסית (Basic Auth) היא שיטה לשליחת מידע מסוים בבקשת HTTP תוך הגנה על המידע. מתכנתים עשויים לטפל בכך כאשר דרושה אותנטיקציה תרשימית בזמן גישה ל-API של שירות.

## איך לעשות:
הנה דוגמה לקוד ב-Java ששולח בקשת HTTP עם אוטנטיקציה בסיסית:

```Java
import java.net.URL;
import java.net.HttpURLConnection;
import java.util.Base64;

public class Main {
    public static void main(String[] args) throws Exception {
        URL url = new URL("http://mywebsite.com");
        String userpass = "username" + ":" + "password";
        String basicAuth = "Basic " + Base64.getEncoder().encodeToString(userpass.getBytes());

        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestProperty ("Authorization", basicAuth);
        int responseCode = connection.getResponseCode();
        System.out.println("Response Code : " + responseCode);
    }
}
```
בקוד הזה, "username" ו-"password" הם פרטי הכניסה שלכם לאתר. התשובה שנחזור היא "Response Code", קוד המציין את תגובת השרת לבקשה שלך.

## צלילה עמוקה
HTTP Basic Auth הוא מקניה עתיקה, בשימוש נרחב מאז שנות ה-90. אף על פי זאת, זו עדיין דרך פשוטה ומהירה לאמת משתמשים, למרות שהיא מציעה הגנה טכנית מועטה. כלומר, האוטנטיקציה מתבצעת באמצעות תוכנת לקוח השולחת שם משתמש וסיסמה מוצפנים בקידומת "Basic". 

לדרך זו יש חלופות מודרניות יותר כמו OAuth2 וJWT שעשויות להציע יותר בטחון, גמישות, ומאפיינים נוספים.

## ראה גם
חפשו מדריכים נוספים להגנה ואותנטיקציה ב-Java, העצמו את הבנייה שלכם של בקשות HTTP באמצעות ספריה פופולרית כמו [OkHttp](https://square.github.io/okhttp/), ולמדו על [OAuth2](https://oauth.net/2/) ו[JWT](https://jwt.io/introduction/) כחלופות ל-Basic Auth.