---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:01:03.559391-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי מאפשרת לנו להתחבר בטוח לשרתים ולגשת למידע שמוגן. תכניתנים עושים זאת כדי לאמת משתמשים ולשמור על נתונים רגישים.

## איך לעשות:
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
const char* serverName = "http://yourserver.com";
const char* httpUsername = "user";
const char* httpPassword = "pass";

WiFiClient client;
HTTPClient http;

void setup() {
  Serial.begin(115200);
  
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi...");
  }
  
  if (http.begin(client, serverName)) {
    http.setAuthorization(httpUsername, httpPassword);
    int httpCode = http.GET();

    if (httpCode > 0) {
      String payload = http.getString();
      Serial.println(httpCode);
      Serial.println(payload);
    } else {
      Serial.println("Error on HTTP request");
    }

    http.end();
  }
}

void loop() {
  // Keep the loop empty if you don't need to perform actions continuously
}
```
זה דוגמה פשוטה לקוד שמתחבר ל-WiFi ושולח בקשת GET עם אימות בסיסי.

## צלילה לעומק
בדיוק כמו בדפדפן, גם המיקרו-בקרים שלנו צריכים להיות מאומתים בפני שרתים. אימות בסיסי ב-HTTP הוא חלק ממודל אבטחת האינטרנט מאז שנות ה-90. גישה אלטרנטיבית לאימות היא OAuth, שהוא יותר מאובטח אך גם מורכב יותר להטמעה. בבקשה שלנו, אנחנו שולחים שם משתמש וסיסמה בצורה מקודדת בבייס64 בכותרת Authorization.

## ראה גם
- מדריך ESP8266 HTTPClient: https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/client-examples.html
- מידע על אימות בסיסי ב-HTTP: https://tools.ietf.org/html/rfc7617
- אימות בטוח יותר עם OAuth: https://oauth.net
