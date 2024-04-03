---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:53.100427-07:00
description: "YAML, \u05E9\u05DE\u05E1\u05DE\u05DC \"YAML Ain't Markup Language\"\
  \ (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF\
  ), \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D8\u05E7\u05E1\u05D8 \u05D1\
  \u05E8\u05D5\u05E8 \u05DC\u05D0\u05D3\u05DD \u05DC\u05E1\u05E8\u05D9\u05D0\u05DC\
  \u05D9\u05D6\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\
  \u05DD \u05D1\u05D5 \u05DC\u05E7\u05D5\u05D1\u05E6\u05D9 \u05E7\u05D5\u05E0\u05E4\
  \u05D9\u05D2\u05D5\u05E8\u05E6\u05D9\u05D4,\u2026"
lastmod: '2024-03-13T22:44:39.869815-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u05E9\u05DE\u05E1\u05DE\u05DC \"YAML Ain't Markup Language\" (YAML\
  \ \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF), \u05D4\
  \u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D8\u05E7\u05E1\u05D8 \u05D1\u05E8\
  \u05D5\u05E8 \u05DC\u05D0\u05D3\u05DD \u05DC\u05E1\u05E8\u05D9\u05D0\u05DC\u05D9\
  \u05D6\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך ל:
כדי לעבוד עם YAML ב-C++, בחירה פופולרית היא הספרייה `yaml-cpp`. ראשית, וודאו ש-`yaml-cpp` מותקן ומקושר כראוי לפרויקט ה-C++ שלכם.

**קריאת קובץ YAML:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Title: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

נתון `config.yaml` הנראה כך:

```yaml
title: "Example YAML"
```

הרצת הקוד ה-C++ לעיל תפיק:

```
Title: Example YAML
```

**כתיבה לקובץ YAML:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Example YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

הקוד הזה ייצור `output.yaml` עם התוכן:

```yaml
title: "Example YAML"
```

הדוגמאות הללו מספקות הקדמה בסיסית לקריאה מתוך וכתיבה לקבצי YAML ב-C++ באמצעות הספרייה `yaml-cpp`. למבנים מורכבים יותר ומקרי שימוש, חקרו את התיעוד של `yaml-cpp` לפיצ'רים כמו סדרות, תגים, וטכניקות סריאליזציה ודיסריאליזציה מתקדמות יותר.
