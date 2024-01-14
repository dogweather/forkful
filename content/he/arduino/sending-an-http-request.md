---
title:                "Arduino: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

ביצוע בקשות HTTP יכול להיות מאוד שימושי בפרויקטי ארדואינו. למשל, אם אתה רוצה לקבל מידע מאתר אינטרנט או לשלוח נתונים לשרת, בקשות HTTP מאפשרות לך לעשות זאת בקלות ובמהירות. 

## איך לעשות זאת

על מנת לשלוח בקשת HTTP, ניצור מקש על הלוח שלנו כדי להפעיל את התכנית. למשל, אם ברצונך לשלוח בקשת GET לאתר אינטרנט מסוים, תוכל לעשות זאת כך:

```arduino
#include <WiFi.h>
#include <WiFiClient.h>

char server[] = "www.example.com";
WiFiClient client;

void setup() {
  Serial.begin(9600);
  WiFi.begin("SSID", "password"); // הכנס את שם הרשת והסיסמה שלך כאן
  Serial.print("Connecting to WiFi");
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println();
  Serial.print("Connected to WiFi: ");
  Serial.println(WiFi.localIP());
}

void loop() {
  if (client.connect(server, 80)) { // התחברות לשרת בפורט 80
    Serial.println("Connected to server");
    client.print("GET / HTTP/1.1\r\n"); // שליחת בקשת GET
    client.print("Host: www.example.com\r\n");
    client.print("Connection: close\r\n\r\n");
    while (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }
  client.stop();
  while(1);
}
```

כאן אנו רואים כי התחברנו לאינטרנט ושלחנו בקשת GET. התוכנית תדפיס את התגובה המלאה של האתר בטרם היא נסגרת. כמו כן, יש לוודא שנסגרת הלולאה שליצת התוכנית כדי שהתכנית תרוץ רק פעם אחת.

## אילוף עמוק

בשלב הזה אתה כבר יודע כיצד לבצע בקשות HTTP באמצעות ארדואינו. אתה יכול לשנות את התכנית כדי לאפשר את השליחה של נתונים כמו משתנים שלנו אל השרת. בנוסף, אתה יכול לחקור עוד אקטיבים נתחי של בקשות, כגון POST ו- PUT. אתה יכול גם ללמוד עוד על קודי מצבים וכיצד לטפל בתוצאות התגובה שע