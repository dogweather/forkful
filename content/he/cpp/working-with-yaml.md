---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט נתונים שמתוכנן להיות קל לקריאה על ידי בני אדם וניתן לפענוח על ידי מחשבים. תכניתנים עובדים עם YAML כי הוא מאפשר הגדרה פשוטה של עצמים ומבני נתונים, כמו הגדרות תצורה ומודלים.

## איך לעשות:
```C++
#include <iostream>
#include <yaml-cpp/yaml.h>

int main() {
    // קריאת YAML ממחרוזת
    YAML::Node config = YAML::Load("name: Yossi\nage: 30\ncity: Tel Aviv");

    // שליפת ערכים מה-YAML
    std::string name = config["name"].as<std::string>();
    int age = config["age"].as<int>();

    // הצגת הערכים
    std::cout << "Name: " << name << "\nAge: " << age << std::endl;

    return 0;
}
```
פלט דוגמא:
```
Name: Yossi
Age: 30
```

## עיון מעמיק:
YAML, שמקוצר מ-"YAML Ain't Markup Language", הומצא בתחילת שנות ה-2000 כתחליף נוח יותר ל-XML. אלטרנטיבות כוללות את JSON, XML, ו-INI. כשמשתמשים ב-YAML ב-C++, יש צורך בספריית חיצונית, כמו yaml-cpp, שתומכת בפענוח והערכת קבצי YAML בקוד קומפקטי ובטוח.

## ראה גם:
- דוקומנטציה של הספרייה yaml-cpp: https://github.com/jbeder/yaml-cpp
- YAML דוקומנטציה רשמית: https://yaml.org/
- מדריך ל-YAML: https://learnxinyminutes.com/docs/yaml/
