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

## מה ולמה?
שליחת בקשת HTTP היא פעולה שמאפשרת למכשירים לשלוח ולקבל מידע מעם אחד למחשב אחר באמצעות האינטרנט. תכניתנים עושים זאת בכדי לשלוט על המכשירים או לצבור מידע חיוני עבור יישומים שונים.

## איך לעשות:
כדי לשלוח בקשת HTTP מתוך קוד Arduino, ישנם שלושה שלבים עיקריים:
1. להתחבר לרשת WiFi שלך ע"י שימוש בקוד קובץ שמתאים למכשיר.
2. ליצור חיבור TCP עם השרת שבו אתה רוצה לשלוח את הבקשה.
3. ליצור ולשלוח את הבקשת HTTP באמצעות החיבור הזה.

כאן אתה יכול לראות דוגמאות של קוד Arduino לשליחת בקשת GET ו-POST:
```arduino
// דוגמא לקוד GET
#include <WiFi.h>

char ssid[] = "WiFi-SSID"; // הזן את שם הרשת שלך
char password[] = "WiFi-Password"; // הזן את ססמת הרשת שלך
char server[] = "example.com"; // הזן את כתובת הדומיין של השרת שבו אתה רוצה לשלוח את הבקשה
int port = 80; // הזן את יציאת הפורט של השרת
WiFiClient client; // יצירת משתנה חיבור

void setup() {
  Serial.begin(9600);
  WiFi.begin(ssid, password); // חיבור לרשת WiFi
  Serial.print("Connecting");
  while (WiFi.status() != WL_CONNECTED) { // המתנה לחיבור לרשת
    delay(500);
    Serial.print(".");
  };
  Serial.println();
  Serial.print("Connected to network: ");
  Serial.println(WiFi.localIP()); // קבלת כתובת IP
  Serial.println("Sending GET request...");
  if (client.connect(server, port)) { // חיבור לשרת
    client.println("GET / HTTP/1.1"); // שליחת הבקשה
    client.println("Host: example.com"); // הגדרת שם המארח של הבקשה
    client.println("Connection: close"); // סגירת החיבור
    client.println(); // הדפסת שורת שורה ריקה למצב רצף (CRLF)
  }
}

void loop() {
  if (client.available()) { // בדיקת האם יש נתונים מהשרת לקרוא
    char c = client.read(); // קריאת תו קלט
    Serial.print(c); // הדפסת תו למסך
  }
  if (!client.connected()) { // בדיקת האם החיבור סגור
    client.stop(); // סגירת החיבור
    while(1); // יציאה לולאה לכבוד ווי
  }
}

// דוגמא לקוד POST
#include <WiFi.h>

char ssid[] = "WiFi-SSID"; // הזן את שם הרשת שלך
char password[] = "WiFi-Password"; // הזן את ססמת הרשת שלך
char server[] = "example.com"; // הזן את כתובת הדומיין של השרת שבו אתה רוצה לשלוח את הבקשה
int port = 80; // הזן את יציאת הפורט של השרת
WiFiClient client; // יצירת משתנה חיבור
String post_request = "value1=10&value2=20"; // הגדרת אנלגיש לנתוני POST

void setup() {
  Serial.begin(9600);
  WiFi.begin(ssid, password); // חיבור לרשת WiFi
  Serial.print("Connecting");
  while (WiFi.status() != WL_CONNECTED) { // המתנה לחיבור לרשת
    delay(500);
    Serial.print(".");
  };
  Serial.println();
  Serial.print("Connected to network: ");
  Serial.println(WiFi.localIP()); // קבלת כתובת IP
  Serial.println("Sending POST request...");
  if (client.connect(server, port)) { // חיבור לשרת
    client.println("POST / HTTP/1.1"); // שליחת הבקשה
    client.println("Host: example.com"); // הגדרת שם המארח של הב