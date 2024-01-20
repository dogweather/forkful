---
title:                "עובדים עם yaml"
html_title:           "Arduino: עובדים עם yaml"
simple_title:         "עובדים עם yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
לעבוד עם YAML היא תהליך בו מתכנתים עובדים עם מבני נתונים מבוזרים בקוד פתוח. הם עושים זאת כדי לפשט את התהליך של כתיבת קוד ולהבטיח שהקוד יהיה נקי ומבוטל.

## איך לעשות?
בארדוין ישנם מספר דרכים לעבוד עם YAML, והנה כמה דוגמאות:

  ```Arduino
  #include <YAML.h>

  // יצירת מבנה נתונים בפורמט YAML
  YAML::Node data = YAML::LoadFile("data.yaml");

  // כתיבת מבנה נתונים לקובץ YAML חדש
  YAML::Emitter out;
  out << YAML::BeginMap;
  out << YAML::Key << "name" << YAML::Value << "John";
  out << YAML::Key << "age" << YAML::Value << 30;
  out << YAML::EndMap;

  // קריאת ערכים מהמבנה הנתונים
  int age = data["age"].as<int>();

  // שינוי ערך במבנה הנתונים
  data["name"] = "Sarah";

  // הדפסת תוכן המבנה נתונים למסך
  Serial.println(data); 
  ```

## מעומק
YAML פותח בשנת 2001 על ידי קבוצת מתכנתים כדי להוסיף נוחות לתהליך כיום של כתיבת קוד. יתר על כן, ישנן אלטרנטיבות נוספות לעבוד עם YAML כמו למשל JSON וXML. בארדוין, פריסת YAML נעשית על ידי ספריית YAML המאפשרת למתכנתים לעבוד בקלות עם מבני נתונים מבוזרים בקוד פתוח. בנוסף, פורמט YAML מאפשר למתכנתים לטפל במבני נתונים מבוזרים בקוד פתוח בדרך קלה יותר ממה שהוא עושה – ובכך מקל על ההסקת תקינות הקוד.

## ראה גם
למידע נוסף ניתן למצוא בקישורים הבאים:
- אתר הרשמי של פרויקט Arduino: [https://www.arduino.cc/](https://www.arduino.cc/)