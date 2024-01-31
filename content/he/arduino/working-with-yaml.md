---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא תסדיר קל לקריאה להצגת נתונים אשר משמש בתכנות לקובצי תצורה ותוכן. תכנתים משתמשים בו בגלל הקריאות הגבוהה והקלות לכתיבה ופענוח, בניגוד ל-JSON או XML.

## איך לעשות:
תקשורת עם YAML ב-Arduino אינה אפשרית בצורה ישירה, מכיוון שמדובר במיקרו-קונטרולרים עם משאבים מוגבלים ו-YAML מיועד יותר לשרתים ולמחשבים אישיים. במקום זאת, אפשר להשתמש בפורמטים יותר חסכוניים כמו JSON.

```Arduino
// לדוג' אין קוד ספציפי ל-YAML ב-Arduino
```

אבל, קוד לשימוש ב-JSON:

```Arduino
#include <ArduinoJson.h>

void setup() {
    Serial.begin(9600);

    const char* json = "{\"sensor\":\"gps\",\"time\":1351824120}";

    DynamicJsonDocument doc(1024);
    deserializeJson(doc, json);

    const char* sensor = doc["sensor"];
    long time = doc["time"];

    Serial.println(sensor);
    Serial.println(time);
}

void loop() {
    // פונקציית ה-loop אינה מבצעת פעולות בדוגמה זו
}
```

תוצאה:
```
gps
1351824120
```

## עומק הצלילה
YAML, שמעמיד ישראל יתיר בקצה ימינו Yet Another Markup Language, הופך לתקן פופולרי לכתיבת קבצי תצורה מאז שנות ה-2000 בזכות קלות השימוש והקיבולת הרחבה של סוגי נתונים. גרסאות קודמות של קבצי תצורה כללו INI ו-XML, אך הם נחשבים כבדים ופחות קריאים. YAML מאפשר ייצוג של מבנה נתונים מורכב בצורה פשוטה וברורה, כך שקל להבינו גם ללא רקע טכני.

בעולם אינטרנט הדברים ומעגלי המיקרו-קונטרולר, כמו Arduino, יש צורך בתקשורת יעילה וקומפקטית. לכן, פורמטים כמו JSON או תקשורת בינארית נעשים שימוש בהם בעיקר בהתבסס על המגבלות ההאדרוואריות והנפח התעבורתי הנמוך של המכשירים.

## לראות גם
- המסמך המקיף ל-YAML: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- ArduinoJson, ספריית JSON ל-Arduino: [https://arduinojson.org/](https://arduinojson.org/)
- מידע על סוגי נתונים בארדואינו: [https://www.arduino.cc/reference/en/](https://www.arduino.cc/reference/en/)
