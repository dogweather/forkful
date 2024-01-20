---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# מה ולמה?
שליחת בקשת HTTP באמצעות אימות בסיסי היא אחת מן השיטות להעביר נתונים בין רכיבים של מערכת. מתכנתים משתמשים בה לתקשר עם שרתים ולשלוט על הנתונים שהם מוסרים ומקבלים.

# איך לעשות:
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "שם_הרשת";
const char* password =  "סיסמה";

void setup() {

  Serial.begin(115200);

  // התחברות לרשת עם שם וסיסמה
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("מתחבר לרשת...");
  }
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) {   
    HTTPClient http;

    http.begin("http://your.server.com");  
    http.addHeader("Content-Type", "application/x-www-form-urlencoded");
    http.setAuthorization("שם_משתמש", "סיסמה");  

    int httpCode = http.POST("your data to send");
    String payload = http.getString();    

    Serial.println(httpCode);
    Serial.println(payload);
  }
}
```
הפלט, לדוגמה:
```
200
הפלט מהשרת
```

# עומק
שליחת בקשת HTTP באמצעות אימות בסיסי היא מרכיב מרכזי באינטרנט מאז שהוא נוצר. ישנם גם שיטות אלטרנטיביות, כמו אימות Digest או אימות Bearer, אך הם מסובכים יותר. השיטה המוצגת כאן משתמשת בספריית ESP8266HTTPClient של Arduino, שמסתפקת בהוספת שורה אחת עם השם משתמש והסיסמה לבקשה.

# עוד מקורות למידע
2. [מידע על אימות בסיסי](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)