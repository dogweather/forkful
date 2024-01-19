---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשה HTTP היא שיטה בה מחשב שולח בקשה לשרת ובחזרה מקבל מידע. מתכנתים משתמשים בזה כדי לגשת ולשלוט במידע מרחוק, כמו מציאת מזג האוויר או שליטה בחכמה הביתית.

## איך לעשות:
ראשית, אנחנו צריכים להגדיר את הספריה הנדרשת:
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>
```

הגדר את פרטי ה-WiFi שלך:
```Arduino
const char* ssid = "שם הרשת שלך";
const char* password = "סיסמה";
```

התחבר ל-WiFi:
```Arduino
WiFi.begin(ssid, password);

while (WiFi.status() != WL_CONNECTED) {
  delay(1000);
  Serial.println("מתחבר ל-WiFi..");
}

Serial.println("מחובר ל-WiFi");
```

שלח בקשת HTTP:
```Arduino
if (WiFi.status() == WL_CONNECTED) {
  HTTPClient http;

  http.begin("http://example.com"); //Specify destination
  int httpCode = http.GET(); //השג את הקוד של התגובה

  if (httpCode > 0) { //בדוק שהקוד תקני
    String payload = http.getString(); //השתמש בפקודה הזאת כדי לקבל תגובה
    Serial.println(payload);
  }

  http.end(); //סגור את החיבור
}
```

## צלילה עמוקה
הבקשות HTTP מרכזיות בשימוש המחשב, בלי קשר לשפה או למערכת ההפעלה. טכנולוגיה זו משמשת באינטרנט הגלובלי מאז שנות ה-90.

אלטרנטיבות? תכנית MQTT היא אלטרנטיבה נפוצה לשליחת בקשות HTTP, שמתאימה במיוחד למכשירים עם ממשקים מוגבלים.

נשאר לנו לשקול שבקשה HTTP רגילה מחייבת חיבור ישיר לאינטרנט. במחשבים רגילים זה לא בעיה, אבל במיקרוקונטרולרים הדבר מהווה בעיה שצריך לטפל בה.

## גם אתה יכול לראות
הנה כמה משאבים שיכולים לעזור לך להעמיק את הידע שלך:
- [HTTP - Wikipedia](https://he.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
- [Arduino - HTTPClient Library](https://www.arduino.cc/en/Tutorial/LibraryExamples/HttpClient)
- [Arduino - MQTT](https://www.arduino.cc/reference/en/libraries/mqtt/)