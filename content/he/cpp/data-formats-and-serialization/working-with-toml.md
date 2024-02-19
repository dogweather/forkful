---
aliases:
- /he/cpp/working-with-toml/
date: 2024-01-26 04:20:16.225118-07:00
description: "TOML (Tom's Obvious, Minimal Language - \u05E9\u05E4\u05EA \u05D4\u05EA\
  \u05DB\u05E0\u05D5\u05EA \u05D4\u05D1\u05E8\u05D5\u05E8\u05D4 \u05D5\u05D4\u05DE\
  \u05D9\u05E0\u05D9\u05DE\u05DC\u05D9\u05EA \u05E9\u05DC \u05D8\u05D5\u05DD) \u05D4\
  \u05D9\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E1\u05E8\u05D9\u05D0\u05DC\u05D9\
  \u05D6\u05E6\u05D9\u05D4 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05E0\u05D5\
  \u05D7 \u05DC\u05E7\u05E8\u05D9\u05D0\u05D4 \u05D1\u05D6\u05DB\u05D5\u05EA \u05D4\
  \u05E1\u05DE\u05E0\u05D8\u05D9\u05E7\u05D4 \u05D4\u05D1\u05E8\u05D5\u05E8\u05D4\
  \ \u05E9\u05DC\u05D4. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
lastmod: 2024-02-18 23:08:53.183436
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language - \u05E9\u05E4\u05EA \u05D4\u05EA\u05DB\
  \u05E0\u05D5\u05EA \u05D4\u05D1\u05E8\u05D5\u05E8\u05D4 \u05D5\u05D4\u05DE\u05D9\
  \u05E0\u05D9\u05DE\u05DC\u05D9\u05EA \u05E9\u05DC \u05D8\u05D5\u05DD) \u05D4\u05D9\
  \u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E1\u05E8\u05D9\u05D0\u05DC\u05D9\u05D6\
  \u05E6\u05D9\u05D4 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05E0\u05D5\u05D7\
  \ \u05DC\u05E7\u05E8\u05D9\u05D0\u05D4 \u05D1\u05D6\u05DB\u05D5\u05EA \u05D4\u05E1\
  \u05DE\u05E0\u05D8\u05D9\u05E7\u05D4 \u05D4\u05D1\u05E8\u05D5\u05E8\u05D4 \u05E9\
  \u05DC\u05D4. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
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
