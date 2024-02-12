---
title:                "עבודה עם JSON"
aliases:
- /he/arduino/working-with-json/
date:                  2024-02-03T19:22:08.378990-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

JSON, או JavaScript Object Notation, הוא פורמט להחלפת נתונים קליל, הופך אותו למושלם לאחסון נתונים או קבצי תצורה בפרויקטים של Arduino. תכנתים משתמשים בו בשל פשטותו וקריאותו במגוון סביבות תכנות, כולל Arduino, מאפשר החלפת נתונים בצורה חלקה עם API-ים של אתרי אינטרנט או מערכות אחרות.

## איך לעשות:

לעבוד עם JSON ב-Arduino, הספרייה `ArduinoJson` היא בחירה פופולרית בשל נוחות השימוש והיעילות שלה. היא מאפשרת ניתוח מחרוזות JSON, שינוים בהן, וסידור מחדש של אובייקטים למחרוזות JSON. כך משתמשים בה:

1. **להתקין את ספריית ArduinoJson**: השתמשו במנהל הספריות בסביבת הפיתוח של Arduino והתקינו "ArduinoJson".

2. **הפיכת מחרוזת JSON לאובייקט**: כך מנתחים מחרוזת JSON ומוציאים ממנה ערכים.

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // התאימו את הגודל בהתאם למסמך JSON
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float latitude = doc["data"][0]; // 48.756080
  float longitude = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // לולאה ריקה
}
```

דוגמת פלט:

```
gps
1351824120
48.756080
2.302038
```

3. **המרה למחרוזת JSON**: כך יוצרים מחרוזת JSON מנתונים.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // התאימו את הגודל בהתאם לנתונים
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // לולאה ריקה
}
```

דוגמת פלט (מעוצבת לקריאות):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

השימוש בספריית `ArduinoJson` מאפשר לפרויקטים של Arduino לתקשר מבני נתונים מורכבים בפורמט קריא לאדם, ומקל על הפיתוח והשילוב עם שירותי אינטרנט.
