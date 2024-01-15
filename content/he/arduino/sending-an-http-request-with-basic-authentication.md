---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Arduino: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

טכנולוגיה המתקדמת והמשתנה בהירות של מכשירי האלקטרוניקה משפיעה על צורת חיים מודרנית. אחד הדרכים הפשוטות והמהירות להתקשרות עם רשת האינטרנט היא ע"י שליחת בקשת HTTP עם אימות בסיסי. במאמר זה נלמד כיצד לעשות זאת באמצעות מכשיר ארדוינו.

## מדוע

קיים מספר סיבות שבהן ניתן להשתמש בשליחת בקשת HTTP עם אימות בסיסי במכשיר ארדוינו. למשל, כאשר נרצה לקבל נתונים מכמה מקורות שונים באינטרנט או לשלוח נתונים למכשיר אחר דרך הרשת, ניתן להשתמש בפרוטוקול זה על מנת לבצע אימות ולוודא שרק מי שמורשה יוכל לגשת לנתונים.

## כיצד לעשות זאת

התחברו לרשת WiFi עם הארדוינו והתקינו את הספרייה המתאימה לשליחת בקשות HTTP. להלן דוגמאות קוד ופלט מוצגים בבלוקי קוד ```Arduino```.

### שליחת בקשת GET

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

void setup() {
  Serial.begin(9600);
  // התחבר לרשת WiFi
  WiFi.begin("SSID", "Password");
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
  }

  // בנה את הכתובת לבקשת GET עם אימות בסיסי
  String url = "https://www.example.com/api/data";
  String auth = base64Encode("username:password");
  String authorization = "Basic " + auth;

  // שלח את הבקשה וקבל את התוצאה
  HTTPClient http;
  http.begin(url);
  http.addHeader("Authorization", authorization);
  int responseCode = http.GET();
  String response = http.getString();

  // עדכן את המידע בטרמינל
  Serial.print("HTTP Response Code: ");
  Serial.println(responseCode);
  Serial.print("HTTP Response: ");
  Serial.println(response);

  http.end();
}

void loop() {
  // רקורסיה
}
```

#### פלט:
```
HTTP Response Code: 200
HTTP Response: {"temperature": "25", "humidity": "50"}
```

### שליחת בקשת POST

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

String data = "{\"name\": \"John\", \"age\": 30}";

void setup() {
  Serial.begin(9600);
  // התחבר לרשת WiFi
  WiFi.begin("SSID", "Password");
  while (WiFi.status() !=