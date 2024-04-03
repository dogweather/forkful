---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:14.999297-07:00
description: "\u05DB\u05D9\u05E6\u05D3: \u05D1-C++, \u05D0\u05D9\u05DF \u05EA\u05DE\
  \u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC-JSON, \u05D0\u05DA\
  \ \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E9\u05DC \u05E6\u05D3 \u05E9\u05DC\u05D9\
  \u05E9\u05D9 \u05DB\u05DE\u05D5 nlohmann/json \u05D4\u05D5\u05E4\u05DB\u05D5\u05EA\
  \ \u05D0\u05D5\u05EA\u05D5 \u05DC\u05E4\u05E9\u05D5\u05D8. \u05D4\u05E0\u05D4 \u05DB\
  \u05D9\u05E6\u05D3 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D4 \u05DC\u05DE\
  \u05E9\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9\u05D5\u05EA: \u05E8\
  \u05D0\u05E9\u05D9\u05EA, \u05D5\u05D5\u05D3\u05D0\u05D5 \u05E9\u05D4\u05E1\u05E4\
  \u05E8\u05D9\u05D4\u2026"
lastmod: '2024-03-13T22:44:39.871442-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-C++, \u05D0\u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\
  \u05D1\u05E0\u05D9\u05EA \u05DC-JSON, \u05D0\u05DA \u05E1\u05E4\u05E8\u05D9\u05D5\
  \u05EA \u05E9\u05DC \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05DE\u05D5\
  \ nlohmann/json \u05D4\u05D5\u05E4\u05DB\u05D5\u05EA \u05D0\u05D5\u05EA\u05D5 \u05DC\
  \u05E4\u05E9\u05D5\u05D8."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

## כיצד:
ב-C++, אין תמיכה מובנית ל-JSON, אך ספריות של צד שלישי כמו nlohmann/json הופכות אותו לפשוט. הנה כיצד להשתמש בה למשימות בסיסיות:

ראשית, וודאו שהספריה מותקנת. אם אתם משתמשים במנהל חבילות כמו vcpkg או Conan, תוכלו להוסיף בקלות את `nlohmann/json` לפרויקט שלכם.

### פרסור JSON ממחרוזת
```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // נתוני JSON כמחרוזת
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // פרסור מחרוזת JSON
    auto jsonObject = nlohmann::json::parse(jsonData);

    // גישה לנתונים
    std::cout << "Name: " << jsonObject["name"] << "\n"
              << "Age: " << jsonObject["age"] << "\n"
              << "City: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**פלט לדוגמה:**

```
Name: John
Age: 30
City: New York
```

### יצירת JSON
יצירת נתוני JSON היא פשוטה באותה מידה; פשוט מקצים ערכים לאובייקט `nlohmann/json`.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // יצירת אובייקט JSON
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // המרת אובייקט JSON למחרוזת והדפסה
    std::string jsonString = jsonObject.dump(4); // ארגומנט 4 להדפסה יפה
    std::cout << jsonString << std::endl;

    return 0;
}
```

**פלט לדוגמה:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

דוגמאות אלו מדגימות את הפונקציונליות הבסיסית לעבודה עם JSON ב-C++ באמצעות הספריה `nlohmann/json`. עם הידע הבסיסי הזה, תוכלו לנתח וליצור JSON למגוון יישומים, החל מקבצי תצורה ועד החלפת נתונים ביישומים מרושתים.
