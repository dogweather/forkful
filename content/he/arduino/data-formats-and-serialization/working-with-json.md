---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.378990-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC\u05E2\u05D1\
  \u05D5\u05D3 \u05E2\u05DD JSON \u05D1-Arduino, \u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 `ArduinoJson` \u05D4\u05D9\u05D0 \u05D1\u05D7\u05D9\u05E8\u05D4 \u05E4\u05D5\
  \u05E4\u05D5\u05DC\u05E8\u05D9\u05EA \u05D1\u05E9\u05DC \u05E0\u05D5\u05D7\u05D5\
  \u05EA \u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D5\u05D4\u05D9\u05E2\u05D9\u05DC\
  \u05D5\u05EA \u05E9\u05DC\u05D4. \u05D4\u05D9\u05D0 \u05DE\u05D0\u05E4\u05E9\u05E8\
  \u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ JSON, \u05E9\u05D9\u05E0\u05D5\u05D9\u05DD \u05D1\u05D4\u05DF,\u2026"
lastmod: '2024-03-13T22:44:39.803120-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD JSON \u05D1-Arduino, \u05D4\u05E1\
  \u05E4\u05E8\u05D9\u05D9\u05D4 `ArduinoJson` \u05D4\u05D9\u05D0 \u05D1\u05D7\u05D9\
  \u05E8\u05D4 \u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\u05EA \u05D1\u05E9\u05DC\
  \ \u05E0\u05D5\u05D7\u05D5\u05EA \u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D5\u05D4\
  \u05D9\u05E2\u05D9\u05DC\u05D5\u05EA \u05E9\u05DC\u05D4."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

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
