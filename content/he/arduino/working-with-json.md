---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON (JavaScript Object Notation) הוא פורמט נתונים קליל וקריא. מתכנתים משתמשים בו להחלפת נתונים בין חומרה ותוכנה, כמו בין שרת ו-Arduino, בגלל פשטותו ורווחיותו.

## איך לעשות:
```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(115200);

  // JSON טקסט דוגמא
  const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

  StaticJsonDocument<200> doc;
  deserializeJson(doc, json);

  const char* sensor = doc["sensor"];
  long time = doc["time"];
  float latitude = doc["data"][0];
  float longitude = doc["data"][1];

  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // nothing to do here
}
```
תוצאות לדוגמא:
```
gps
1351824120
48.756080
2.302038
```

## נסיגה לעומק:
תקן ה-JSON התפתח מאובייקטים ב-JavaScript אבל הפך לעצמאי ונתמך ברוב שפות התכנות. אלטרנטיבות נוספות כוללות XML ו-YAML. ארדואינו משתמשת בספריית ArduinoJson לניתוח וכתיבה של JSON, מה שדורש מנהלת זיכרון ותכנון מראש של המבנה.

## גם ראה:
- מדריך [ArduinoJson](https://arduinojson.org/)
- מפרט התקניות [JSON](https://www.json.org/json-en.html)
- דוקומנטציה לספרייה [ArduinoJson GitHub](https://github.com/bblanchon/ArduinoJson)
