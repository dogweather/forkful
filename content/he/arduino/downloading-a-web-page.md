---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט היא התהליך שבו מחשב רוכש מידע מהאינטרנט ושומר אותו מקומית. מתכנתים עשויים לבצע את זה כדי לאסוף נתונים, ליישר את תצורת המערכת או לבדוק תקינות שלקוח מקומי.

## איך לעשות:
Arduino IDE תומך בפונקציונליות WebClient, שמאפשרת ליצור קוד שיכול להוריד דפים מהאינטרנט. בלוק הקוד הבא מדגים זאת.

```Arduino
#include <Ethernet.h>
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress ip(192,168,1,177);
char server[] = "www.example.com";
EthernetClient client;

void setup() {
  Ethernet.begin(mac, ip); 
  Serial.begin(9600);
  delay(1000);
  Serial.println("connecting...");

  if (client.connect(server, 80)) {
    Serial.println("connected");
    client.println("GET / HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  }
  else {
    Serial.println("connection failed");
  }
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }

  if (!client.connected()) {
    Serial.println();
    Serial.println("disconnecting.");
    client.stop();
    for(;1;);
  }
}
```
פלט משורת הפקודה: `connecting... connected`

## צלילה עמוקה:
איסוף נתונים מהאינטרנט משמש באופן רחב בפעולות של יישומי IoT, כאשר מכשירים נחשבים באופן דינאמי עם שרתים מרוחקים. קיימות חלופות לשיטת ה-WEB Client, כולל ספריה של WifiClient, שמתמקדת בתמיכה ב- WiFi. מעבר לכך, מספר פרטי ביצוע משתנים בהתאם לסוג השרת ולאופן שבו המידע מורגש (קובץ HTML, XML, JSON וכו').

## ראו גם:
1. [מסמך עזר ל-WEB Client של Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/WebClient)
2. [הסבר על שימוש ב-WIFI Client](https://www.arduino.cc/en/Tutorial/WiFiWebClient)
3. [תיעוד של שרת הרשת של Arduino](https://www.arduino.cc/en/Tutorial/WebServer)