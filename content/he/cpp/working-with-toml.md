---
title:                "עבודה עם TOML"
date:                  2024-01-26T04:20:16.225118-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
TOML (Tom's Obvious, Minimal Language - שפת התכנות הברורה והמינימלית של טום) היא פורמט סריאליזציה נתונים שנוח לקריאה בזכות הסמנטיקה הברורה שלה. תכנתים משתמשים ב-TOML עבור קבצי קונפיגורציה מכיוון שהיא מציעה איזון בין קריאות אנושית לניתוח מכונה.

## איך ל:
כדי לעבוד עם TOML ב-C++, תצטרך ספרייה כמו `toml++`. הנה התחלה מהירה:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // ניתוח TOML מקובץ
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // גישה לערך
    std::string title = config["title"].value_or("Untitled");
    std::cout << "Title: " << title << '\n';

    // שינוי ושמירת TOML
    config["title"] = "New Title";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

דוגמת `config.toml`:
```toml
title = "Example"
```

פלט לדוגמא:
```plaintext
Title: Example
```

## עיון נוסף
TOML נוצר על ידי טום פרסטון-וורנר ב-2013 כחלופה ל-YAML ול-JSON. הוא תוכנן להיות פשוט ומפורש, בעיקר עבור קבצי קונפיגורציה. בניגוד ל-JSON, TOML מתמקד בלהיות חד-משמעי, כלומר הוא דטרמיניסטי באופן פרסום המסמך.

חלופות ל-TOML כוללות את YAML, שהיא יותר רחבת אפשרויות במה שמותר, לעיתים על חשבון הניבויות. JSON, חלופה נוספת, היא די נוקשה במבנה אך לא כל כך ידידותית לאנוש בתצורות מכיוון שאין אפשרות להערות והתחביר שלה מלא בסוגריים מסולסלים.

בביצוע, `toml++` היא ספרייה של C++17 המכילה רק כותרות אשר מתאימה למפרט TOML האחרון. היא מספקת ממשק דומה ל-DOM לניווט ולשינוי נתוני TOML, מה שהופך אותה לפשוטה לשילוב בפרויקטים. הספרייה דואגת לעיבוד, אימות וייצור הפלט, מה שמאפשר לך לקבל ולהגדיר נתוני TOML באמצעות סוגים של C++.

## ראו גם
- מאגר ה-GitHub של TOML: https://github.com/toml-lang/toml
- `toml++`, ספרייה של C++ עבור TOML: https://github.com/marzer/tomlplusplus
- התיעוד הרשמי של TOML עם הסברים מפורטים על הפורמט: https://toml.io/en/
