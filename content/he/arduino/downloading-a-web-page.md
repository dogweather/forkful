---
title:                "Arduino: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# למה:
למה להתעסק בהורדת עמוד אינטרנט? טכנולוגיית ארדואינו מאפשרת לנו להפוך כל מכשיר שמתוכנת לכולל לרשת האינטרנט. זה מאפשר לנו להיות מחוברים ולהתחבר לכל מיני מידע ושירותים ברחבי העולם.

# איך לעשות זאת:
תחילה, יש להתקין את ספריית רשת האינטרנט עבור ארדואינו - "Ethernet". לאחר מכן, ניתן ליצור חיבור רשת על ידי שימוש בפונקציות כמו "begin()" ו "connect()". לבסוף, נוכל להשתמש בפונקציות "readString()" ו"println()" כדי לקבל תוכן של עמוד אינטרנט ולהציג אותו בטכנולוגיית הארדואינו.

```arduino
#include <Ethernet.h>

char server[] = "www.example.com"; // אתר אינטרנט מטרה
int port = 80; // ייצוג עבור כניסה לרשת

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; // מספר הרשת של הארדואינו (MAC address)
IPAddress ip(192, 168, 1, 177); // כתובת ה-IP של הארדואינו ברשת

EthernetClient client;

void setup() {
  Ethernet.begin(mac, ip);
  Serial.begin(9600);
  while (!Serial) {
    ; // במידה ואין חיבור תקין למחשב, נמשיך לחכות לחיבור
  }
}

void loop() {
  if (client.connect(server, port)) {
    Serial.println("Connected to server!");
    client.println("GET /index.html HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println(); // קו ריק לפני שנתתחלף לתכנן תוכן מהשרת
  } else {
    // אם לא מתאם חיבור לשרת
    Serial.println("Connection failed!");
  }
  
  while (client.connected()) {
    // במידה ורשת מחוברת
    if (client.available()) {
      // אם יש תוכן מזוהה בבלוק, הדפיסו אותו
      String line = client.readStringUntil('\n');
      Serial.println(line);
    }
  }
  
  delay(10000); // מאחר ופקודת סקירה והדפסת תוכן נעשית בפעם בערך כל 10 שניות
}

```

# חפירה עמוקה:
הורדת עמוד אינטרנט היא תה