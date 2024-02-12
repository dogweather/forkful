---
title:                "עבודה עם YAML"
aliases:
- /he/arduino/working-with-yaml.md
date:                  2024-02-03T19:25:37.447984-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
