---
title:                "שליחת בקשת Http עם אימות בסיסי"
html_title:           "Arduino: שליחת בקשת Http עם אימות בסיסי"
simple_title:         "שליחת בקשת Http עם אימות בסיסי"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא פעולה שמאפשרת למתכנתים לשלוח תוכן או לבצע פעולות על שרתים מרחוק באמצעות פרוטוקול ה-HTTP. מתכנתים משתמשים בפעולה זו לשלוח מידע לשרתי אינטרנט כמו אתרים או אפליקציות דרך קוד תכנותי בסביבת Arduino.

## איך לעשות?
```Arduino
#include <ESP8266WiFi.h> // ייבוא ספריות
#include <ESP8266HTTPClient.h>

const char* ssid = "WiFi_שלך"; // הכנס כאן את ה-SSID של רשת ה-WiFi
const char* password = "סיסמת_רשת_ה-WiFi_שלך"; // הכנס כאן את סיסמת רשת ה-WiFi

void setup() {
  WiFi.begin(ssid, password); // התחברות ל-WiFi
  while (WiFi.status() != WL_CONNECTED) { // המתן להתחברות
    delay(500);
  }
  if(WiFi.status() == WL_CONNECTED){ // בדיקה שהתחברנו בהצלחה
    Serial.begin(115200); // פתיחת קומוניקציה עם מחשב במהירות 115200
    HTTPClient http; // יצירת אובייקט שישמש לבצע את הבקשה HTTP
    http.begin("https://www.example.com"); // הכנס כאן את הכתובת של השרת שאליו תרצה לשלוח את הבקשה
    http.setAuthorization("username", "password"); // הכנס כאן את שם המשתמש והסיסמה לאימות בקשת HTTP
    int httpResponseCode = http.GET(); // שליחת בקשת GET וקבלת תשובת הקוד מהשרת
    String payload = http.getString(); // קבלת המידע שנשלח מהשרת ושימוש במשתנה payload לאחסונו
    Serial.println(httpResponseCode); // הדפסת תשובת הקוד מהשרת
    Serial.println(payload); // הדפסת המידע שקיבלנו מהשרת
    http.end(); // סיום סשן HTTP
  }
}

void loop() {
  // כאן נמשיך לעשות פעולות נוספות עם המידע שקיבלנו מהשרת
}
```

## צלילה מעמוקה
- היסטוריה: שליחת בקשת HTTP עם אימות בסיסי מתאפשרת דרך הפרוטוקול הפשוט להעברת הפרוטוקולים (HTTP) שנוצר בשנת 1991 על ידי טים ברנרס לצורך שיתוף תוכן בין מחשבים. השימוש באימות בסיסי נוצר כדי להשלים את פעולות האימות בעת שליחת בקשות HTTP.
- אלטרנטיבות: ישנן שיטות אלטרנטיביות לאימות בסיסי בשרתי האינטרנט, כגון אימות OAuth או טוקן.
- פרטי רישום: בשליחת בקשות HTTP עם אימות בסיסי, יש לוודא כי שם המשתמש והסיסמה בקישוריות מאובטחת. מומלץ להשתמש בקישוריות מאובטחת (HTTPS) כדי להגן על המידע המועבר בין המכשיר והשרת.

## ראה גם
- מדריך לשליחת דוא"ל עם אימות בסיסי בשביל קוד Arduino: https://create.arduino.cc/projecthub/tremotoz/email-with-basic-authentication-for-arduino-code-c9d41e
- קוד דוגמה לשליחת בקשת HTTP עם אימות בסיסי על גבי NodeMCU: