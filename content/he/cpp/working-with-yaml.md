---
title:                "עבודה עם YAML"
aliases:
- he/cpp/working-with-yaml.md
date:                  2024-02-03T19:24:53.100427-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

YAML, שמסמל "YAML Ain't Markup Language" (YAML אינו שפת סימון), הוא פורמט טקסט ברור לאדם לסריאליזציה של נתונים. מתכנתים משתמשים בו לקובצי קונפיגורציה, דאמפינג של נתונים, ושמירת נתונים היררכיים בזכות הקריאות והתחביר הקל להבנה לעומת XML או JSON.

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
