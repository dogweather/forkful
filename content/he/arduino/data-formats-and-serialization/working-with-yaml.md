---
aliases:
- /he/arduino/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:37.447984-07:00
description: "YAML (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\
  \u05D5\u05DF) \u05D4\u05D9\u05D0 \u05EA\u05E7\u05DF \u05E1\u05D9\u05D3\u05D5\u05E8\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\u05D3\
  \u05DD, \u05D4\u05E0\u05D9\u05EA\u05DF \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\
  \u05E7\u05D1\u05E6\u05D9 \u05E7\u05D5\u05E0\u05E4\u05D9\u05D2\u05D5\u05E8\u05E6\u05D9\
  \u05D4, \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA \u05D1\u05D9\u05DF-\u05EA\u05DB\u05E0\
  \u05D9\u05D5\u05EA, \u05D5\u05D0\u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E4\u05D5\u05E0\u05D9\
  \u05DD \u05DC-YAML\u2026"
lastmod: 2024-02-18 23:08:53.136305
model: gpt-4-0125-preview
summary: "YAML (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\
  \u05D5\u05DF) \u05D4\u05D9\u05D0 \u05EA\u05E7\u05DF \u05E1\u05D9\u05D3\u05D5\u05E8\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\u05D3\
  \u05DD, \u05D4\u05E0\u05D9\u05EA\u05DF \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\
  \u05E7\u05D1\u05E6\u05D9 \u05E7\u05D5\u05E0\u05E4\u05D9\u05D2\u05D5\u05E8\u05E6\u05D9\
  \u05D4, \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA \u05D1\u05D9\u05DF-\u05EA\u05DB\u05E0\
  \u05D9\u05D5\u05EA, \u05D5\u05D0\u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E4\u05D5\u05E0\u05D9\
  \u05DD \u05DC-YAML\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
---

{{< edit_this_page >}}

## מה ולמה?

YAML (YAML אינו שפת סימון) היא תקן סידור נתונים קריא לאדם, הניתן לשימוש בקבצי קונפיגורציה, תקשורת בין-תכניות, ואחסון נתונים. מתכנתים פונים ל-YAML בפרויקטים של Arduino כדי להפשיט את תהליך הקונפיגורציה של היישומים שלהם, ובכך להקל על שינוי פרמטרים מבלי להצטרך לחדור לעומקי הקוד, לשפר את קריאות הקוד, ולהקל על שיתוף הקונפיגורציה.

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
