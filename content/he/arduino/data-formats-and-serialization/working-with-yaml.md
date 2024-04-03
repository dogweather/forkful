---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:37.447984-07:00
description: "\u05DB\u05D9\u05E6\u05D3 \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\
  \u05EA: \u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML \u05D9\u05E9\u05D9\u05E8\
  \u05D5\u05EA \u05E2\u05DC Arduino \u05D0\u05D9\u05E0\u05D4 \u05E4\u05E9\u05D5\u05D8\
  \u05D4 \u05DB\u05DE\u05D5 \u05D1\u05E1\u05D1\u05D9\u05D1\u05D5\u05EA \u05EA\u05DB\
  \u05E0\u05D5\u05EA \u05D1\u05E8\u05DE\u05D4 \u05D2\u05D1\u05D5\u05D4\u05D4 \u05D9\
  \u05D5\u05EA\u05E8, \u05D1\u05E9\u05DC \u05DE\u05D2\u05D1\u05DC\u05D5\u05EA \u05D6\
  \u05D9\u05DB\u05E8\u05D5\u05DF \u05D5\u05D4\u05D9\u05E2\u05D3\u05E8 \u05E1\u05E4\
  \u05E8\u05D9\u05D5\u05EA \u05E2\u05D9\u05D1\u05D5\u05D3 YAML \u05DE\u05D5\u05D1\u05E0\
  \u05D5\u05EA. \u05E2\u05DD \u05D6\u05D0\u05EA,\u2026"
lastmod: '2024-03-13T22:44:39.801537-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML \u05D9\u05E9\u05D9\u05E8\
  \u05D5\u05EA \u05E2\u05DC Arduino \u05D0\u05D9\u05E0\u05D4 \u05E4\u05E9\u05D5\u05D8\
  \u05D4 \u05DB\u05DE\u05D5 \u05D1\u05E1\u05D1\u05D9\u05D1\u05D5\u05EA \u05EA\u05DB\
  \u05E0\u05D5\u05EA \u05D1\u05E8\u05DE\u05D4 \u05D2\u05D1\u05D5\u05D4\u05D4 \u05D9\
  \u05D5\u05EA\u05E8, \u05D1\u05E9\u05DC \u05DE\u05D2\u05D1\u05DC\u05D5\u05EA \u05D6\
  \u05D9\u05DB\u05E8\u05D5\u05DF \u05D5\u05D4\u05D9\u05E2\u05D3\u05E8 \u05E1\u05E4\
  \u05E8\u05D9\u05D5\u05EA \u05E2\u05D9\u05D1\u05D5\u05D3 YAML \u05DE\u05D5\u05D1\u05E0\
  \u05D5\u05EA."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## כיצד לעשות זאת:
עבודה עם YAML ישירות על Arduino אינה פשוטה כמו בסביבות תכנות ברמה גבוהה יותר, בשל מגבלות זיכרון והיעדר ספריות עיבוד YAML מובנות. עם זאת, לפרויקטים הדורשים ניתוח או יצירת YAML, גישה טיפוסית כוללת שימוש במחשב נלווה (כמו Raspberry Pi) או המרת קבצי YAML לפורמט ידידותי יותר ל-Arduino (כמו JSON) באמצעות תסריטים חיצוניים. לצורך הדגמה, בואו נתמקד בגישה האחרונה באמצעות ספרייה פופולרית: ArduinoJson.

**שלב 1:** המר את הקונפיגורציה שלך מ-YAML ל-JSON. תוכל להשתמש בכלים מקוונים או ביותיליטים שורת פקודה כמו `yq`.

קובץ YAML (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

הומר ל-JSON (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**שלב 2:** השתמש בספריית ArduinoJson כדי לנתח את קובץ ה-JSON בסקיצה שלך ב-Arduino. תחילה, עליך להתקין את ספריית ArduinoJson דרך מנהל הספריות בסביבת הפיתוח של Arduino.

**שלב 3:** טען ונתח את ה-JSON בקוד שלך. בשל מגבלות האחסון ב-Arduino, נניח שמחרוזת ה-JSON מאוחסנת במשתנה או קרואה מכרטיס SD.

סקיצת Arduino לדוגמה:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"password\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* password = doc["wifi"]["password"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Password: ");
  Serial.println(password);
}

void loop() {
  // כאן לא נמצא דבר במשל הזה
}
```

פלט לאחר הרצת הסקיצה:
```
SSID: YourSSID
Password: YourPassword
```

הגישה הזו, הכוללת המרה ל-JSON וניצול של ספריית ArduinoJson, מאפשרת טיפול נוח בקונפיגורציית YAML בתוך פרויקטים של Arduino, מתחמקת מניתוח ישיר של YAML על המיקרו-בקר.
