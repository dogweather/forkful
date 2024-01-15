---
title:                "עבודה עם YAML"
html_title:           "C++: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

קבצי YAML הם פורמט נתונים נקי וקריא שמשמש בכדי לאחסן ולהעביר מידע בקלות. כידוע, ניתן לקרוא ולשנות קבצי YAML באמצעות להתאמן בתוכנה בעיקר פיטונית. אך מה אם אנו רוצים לעבוד עם קבצי YAML בשפת תכנות C++? במאמר זה, אנו נלמד כיצד לקרוא, לכתוב ולערוך קבצי YAML בעזרת שפת התכנות הפופולרית הזו.

## איך לעבוד עם YAML בשפת תכנות C++

לפני שנתחיל, הכירו את הספריות הבאות שנחוצות לפעולה עם YAML ב-C++:

```C++
#include <iostream>
#include <fstream>
#include "yaml-cpp/yaml.h"
```

כעת בואו נתחיל לעבוד עם קבצי YAML!

**קריאה של קובץ YAML**

כדי לקרוא קובץ YAML, נשתמש בפונקציה `LoadFile()` מתוך הספריית `yaml-cpp`. בדוגמא הבאה, אנו נקרא קובץ בשם "example.yaml" ונדפיס את התוכנית הפלט:

```C++
YAML::Node doc = YAML::LoadFile("example.yaml");
std::cout << "Name: " << doc["name"].as<std::string>() << std::endl;
std::cout << "Age: " << doc["age"].as<int>() << std::endl;
```

**כתיבת קובץ YAML**

כעת נלמד כיצד ליצור קובץ YAML בעזרת ספריית `yaml-cpp`. נשתמש בפונקציה `Emit()` כדי ליצור תוכן לקובץ ו- `File()` כדי לפתוח אותו לכתיבה:

```C++
YAML::Emitter out;
out << YAML::BeginMap; // פתיחת מפתח
out << YAML::Key << "name"; // הגדרת שם המפתח
out << YAML::Value << "John Doe"; // הגדרת ערך המפתח
out << YAML::Key << "age";
out << YAML::Value << 25;
out << YAML::EndMap; // סגירת מפתח
std::ofstream file("new.yaml");
file << out.c_str(); // הכתיבה של הקובץ לפי סדר המפתחות והערכים שהוגדרו
```

**עריכת קובץ YAML**

על מנת לערוך קובץ YAML קיים כבר, נשתמש בפונקציה `YAML::Node` כדי לקר