---
title:                "שליחת בקשת http"
html_title:           "Arduino: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה
שלום חברים, אם אתם מתעניינים בעולם הטכנולוגיה ואוהבים לשחק עם חומרה, אנחנו ממליצים לכם לנסות לשלוח בקשת HTTP באמצעות ארדואינו. תהליך זה יאפשר לכם לשלוט ולקבל מידע ממכשירים חיצוניים כגון מכשירי חיישן, אינטרנט ועוד.

## כיצד לעשות זאת
לפני שנתחיל, ניצור פרויקט חדש באמצעות תוכנת Arduino IDE ונשמור אותו. כעת נתחיל לכתוב את הקוד לשליחת בקשת HTTP.

בשורת הפתיחה, נכין את המשתנים הבאים:

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "my_wifi_network"; // לשנות לשם הרשת המדויק
const char* password = "my_wifi_password"; // לשנות לסיסמא של הרשת
const char* serverName = "https://www.example.com"; // להחליף לכתובת האתר שלכם
```

זה הזמן להתחבר לרשת WiFi שלכם:

```Arduino
void setup() {
    Serial.begin(115200);
    
    WiFi.begin(ssid, password); // מתחברים לרשת WiFi
    
    while (WiFi.status() != WL_CONNECTED) {
        delay(500);
        Serial.println("Connecting to WiFi...");
    }

    Serial.println("Connected to WiFi network!");
}
```

עכשיו, נגדיר פונקציה המכילה את הלוגיקה לשליחת הבקשה:

```Arduino
void sendRequest() {
    WiFiClient client;

    if (!client.connect(serverName, 80)) { // נעבור לHTTP בפורט 80
        Serial.println("Connection failed!");
        return;
    }

    // שולחים GET בקשה לאתר ואחכ קוראים את התשובה
    client.print(String("GET") + " / HTTP/1.1\r\n" +
                "Host: " + serverName + "\r\n" +
                "Connection: close\r\n\r\n");
    
    while (client.connected()) {
        String line = client.readStringUntil('\n');
        Serial.println(line);
    }

    client.stop();
}
```

ולסיום, נקרא לפונקציה בפונקציית העיקרית setup כדי לשלוח את הבקשה לאתר שציינו:

```Arduino
void setup() {
    // כל הפונקציות הקודמות

    // קריאה לפונקציה לשליחת בקשת HTTP
    sendRequest();
}
```

וזהו! הקוד המלא כולל פונקציות מלאות נראה כך: