---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:19.652073-07:00
description: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML \u05D1\u05E4\u05E8\u05D5\u05D9\
  \u05E7\u05D8\u05D9\u05DD \u05E9\u05DC Arduino \u05D4\u05D5\u05D0 \u05E2\u05DC \u05DE\
  \u05E0\u05EA \u05DC\u05D7\u05DC\u05E5 \u05DE\u05D9\u05D3\u05E2 \u05DE\u05D3\u05E4\
  \u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05DB\u05E9\u05D9\u05E8\u05D9 \u05D4\
  -Arduino \u05E9\u05DC\u05D4\u05DD \u05DC\u05D4\u05EA\u05E7\u05E9\u05E8 \u05E2\u05DD\
  \ \u05D4\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8, \u05D0\u05D5\u05E1\u05E4\u05D9\
  \u05DD \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.764323-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML \u05D1\u05E4\u05E8\u05D5\u05D9\
  \u05E7\u05D8\u05D9\u05DD \u05E9\u05DC Arduino \u05D4\u05D5\u05D0 \u05E2\u05DC \u05DE\
  \u05E0\u05EA \u05DC\u05D7\u05DC\u05E5 \u05DE\u05D9\u05D3\u05E2 \u05DE\u05D3\u05E4\
  \u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05DB\u05E9\u05D9\u05E8\u05D9 \u05D4\
  -Arduino \u05E9\u05DC\u05D4\u05DD \u05DC\u05D4\u05EA\u05E7\u05E9\u05E8 \u05E2\u05DD\
  \ \u05D4\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8, \u05D0\u05D5\u05E1\u05E4\u05D9\
  \u05DD \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## מה ולמה?

פיענוח HTML בפרויקטים של Arduino הוא על מנת לחלץ מידע מדפי אינטרנט. מתכנתים עושים זאת כדי לאפשר למכשירי ה-Arduino שלהם להתקשר עם האינטרנט, אוספים נתונים מאתרי אינטרנט למטרות החל מאוטומציה ביתית ועד ניטור סביבתי.

## איך לעשות:

פיענוח HTML ב-Arduino בדרך כלל דורש ספריות בעלות טביעת רגל מינימלית בשל מגבלות משאבי המכשיר. בחירה פופולרית לגריפת אינטרנט ופיענוח היא שימוש בספריות `ESP8266HTTPClient` ו-`ESP8266WiFi` עבור ESP8266, או את המקבילות שלהם ל-ESP32, בהתחשב בתמיכה המקורית שלהם ביכולות Wi-Fi ובפרוטוקולי HTTP. הנה דוגמה בסיסית לאחזור ופיענוח HTML, בהנחה שאתם עובדים עם ESP8266 או ESP32:

ראשית, כללו את הספריות הדרושות:
```cpp
#include <ESP8266WiFi.h> // עבור ESP8266
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// השתמשו בספריות המקבילות של ESP32 אם אתם משתמשים ב-ESP32

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
```

התחברו לרשת ה-Wi-Fi שלכם:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("מתחבר...");
    }
}
```

בצעו בקשת HTTP ופרסמו חתיכת HTML פשוטה:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //בדקו את מצב החיבור של ה-WiFi
        HTTPClient http;  //הצהירו על אובייקט מסוג HTTPClient

        http.begin("http://example.com");  //ציינו את יעד הבקשה
        int httpCode = http.GET();  //שלחו את הבקשה

        if (httpCode > 0) { //בדקו את קוד החזרה
            String payload = http.getString();   //קבלו את התגובה לבקשה
            Serial.println(payload);             //הדפסו את תגובת הבקשה

            // פרסמו חלק ספציפי, למשל, חילוץ כותרת מהתגובה
            int titleStart = payload.indexOf("<title>") + 7; // +7 לעבור את תגית ה-"<title>"
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("כותרת העמוד: ");
            Serial.println(pageTitle);
        }

        http.end();   //סגירת החיבור
    }

    delay(10000); //בצעו בקשה כל 10 שניות
}
```

תוצאת הדוגמה (בהנחה ש-http://example.com יש מבנה HTML פשוט):
```
מתחבר...
...
כותרת העמוד: Example Domain
```

הדוגמה הזו מדגימה את האחזור של דף HTML וחילוץ תוכן תגית ה-`<title>`. לצורך פיענוח HTML מורכב יותר, ניתן לשקול שימוש בביטויים רגולריים (בזהירות בשל אילוצי זיכרון) או בפונקציות לעיבוד מחרוזות כדי לנווט במבנה ה-HTML. פיענוך מתקדם יותר עשוי לדרוש גישות מתוחכמות יותר, כולל אלגוריתמי פיענוח מותאמים אישית למבנה הספציפי של ה-HTML שאיתו אתם מתמודדים, מאחר והסביבה הסטנדרטית של Arduino אינה כוללת ספריית פיענוח HTML מובנית.
