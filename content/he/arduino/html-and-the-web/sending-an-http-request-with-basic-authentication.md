---
date: 2024-01-20 18:01:03.559391-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05EA \u05DC\u05E0\u05D5 \u05DC\u05D4\u05EA\u05D7\u05D1\u05E8 \u05D1\
  \u05D8\u05D5\u05D7 \u05DC\u05E9\u05E8\u05EA\u05D9\u05DD \u05D5\u05DC\u05D2\u05E9\
  \u05EA \u05DC\u05DE\u05D9\u05D3\u05E2 \u05E9\u05DE\u05D5\u05D2\u05DF. \u05EA\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05DE\u05EA \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D5\u05DC\u05E9\u05DE\u05D5\u05E8 \u05E2\u05DC \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E8\u05D2\u05D9\u05E9\u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.767457-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05EA \u05DC\u05E0\u05D5 \u05DC\u05D4\u05EA\u05D7\u05D1\u05E8 \u05D1\
  \u05D8\u05D5\u05D7 \u05DC\u05E9\u05E8\u05EA\u05D9\u05DD \u05D5\u05DC\u05D2\u05E9\
  \u05EA \u05DC\u05DE\u05D9\u05D3\u05E2 \u05E9\u05DE\u05D5\u05D2\u05DF. \u05EA\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05DE\u05EA \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D5\u05DC\u05E9\u05DE\u05D5\u05E8 \u05E2\u05DC \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E8\u05D2\u05D9\u05E9\u05D9\u05DD."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
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
