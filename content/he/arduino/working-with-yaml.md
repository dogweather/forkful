---
title:                "עבודה עם yaml"
html_title:           "Arduino: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
למה לעסוק בתכנות עם YAML? פשוט – כי לאופני העברה המקשר בין מכשירי החשמל בקיר הוא מבוסס על קובץ YAML הידידותי לחשמלאים.

## How To
כדי להתחיל לעבוד עם YAML בארדואינו, יש למחוק את המחרוזת "```Arduino … ```" שתופיע בקוד שלנו ולאחר מכן להוסיף את הקוד הבא:
```
#include <ArduinoYAML.h>

StaticJsonDocument<200> doc;

bool success = ArduinoYAML.loadFile("/config.yaml", doc);

if (success) {
  Serial.println("Loaded successfully!");
  serializeJson(doc, Serial);
} else {
  Serial.println("Failed to load YAML file");
}
```

התחילו להשתמש בידע שלכם על Arduino כדי ליצור קבצי YAML מתאימים לצרככם וליהנות מהפשטות המובטחת של YAML!

## Deep Dive
YAML הוא פורמט של קובץ מרובה ערכים פשוטים הנהדר לשימוש בתכניות חשמל מתקדמות כמו ארדואינו. הוא מכיל רכיבים שונים המאפשרים לנו ליצור רשומות מתאימות בצורה מיוחדת, כך שהקוד נראה ונקרא הכי טבעי.

כדי ללמוד עוד על YAML וכיצד ניתן להשתמש בו ביחד עם ארדואינו, ניתן לגלות מידע נוסף בכתובת האינטרנט הבאה:
https://yaml.org/spec/1.2/spec.html
https://arduinojson.org/

## See Also
ראו גם:
https://yaml.org/
https://www.arduino.cc/