---
title:                "הורדת דף אינטרנט"
html_title:           "Arduino: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

אנשים מורידים דף אינטרנט מעבורת כדי לקבל מידע מעודכן ולהתחבר לעולם המקוון. זה יכול להיות חלק מפרויקט ארדואינו או פשוט כדי להתחיל ללמוד תכנות עם לוח הפיתוח הזה.

## איך לעשות זאת

כדי להוריד דף אינטרנט באמצעות ארדואינו, נצטרך להשתמש בספריית "WiFiClient". נתחיל על ידי הכנת הקשר אל הרשת המתאימה:

```arduino
#include <ESP8266WiFi.h>      // להתחבר לרשת
#include <WiFiClient.h>       // ספריית הלקוח
WiFiClient client;           // הקושח
```

לאחר מכן, נכניס את הכתובת של הדף המבוקש לתוך קו חדש:

```arduino
...
const char* server = "www.example.com"; // כתובת הדף
```

עכשיו הגענו לחלק שבו אנו יוצרים את החברת tcp ושולחים את בקשת GET עבור הדף. זה יכול להיכשל אם החיבור אינטרנט לא פעיל:

```arduino
...
int port = 80;  // יצירת חיבור לשרת המבוקש
if (!client.connect(server, port)) { // שליחת בקשת GET דרך היחידה wifi שלך
  Serial.println("connection failed");
  return;
}

client.print("GET /index.html HTTP/1.1\r\n"); // אתחול הקשר ושליחת הבקשה
client.print("Host: www.example.com\r\n");
client.print("Connection: close\r\n\r\n");
```

תוך כמה שניות, אנו מקבלים תגובה מהשרת:

```arduino
...
while(client.connected()){  // עד החיבור לשרת
  if (client.available()) { // זמן המתנה עבור התגובה
    ...
    // קבלת התוכן והדפסתו
    char c = client.read();
    Serial.print(c);
    ...
  }
}
```

לאחרונה, אנו תופסים את כל הקבצים שעברו ומדפיסים אותם בקלות:

```arduino
...
while (client.available() > 0) {
  String line = client.readStringUntil('\r\n'); // היו אפשרי עבור תגובתאם
  Serial.print(line);
}
```

## דרוקים עמוק

הורדת דף אינטרנט בארדואינו אינה תהליך פשוט, בעיקר בגלל המ