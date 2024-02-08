---
title:                "עבודה עם JSON"
aliases:
- he/cpp/working-with-json.md
date:                  2024-02-03T19:22:14.999297-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

JSON (תיאור אובייקט של JavaScript) הוא פורמט קל משקל לאחסון והעברת נתונים, מה שהופך אותו לתווך מצוין להחלפת נתונים בין שרתים ליישומי ווב. מתכנתים משתמשים ב-JSON עקב הקריאות הקלה שלו על ידי בני אדם והפרסור הפשוט שלו על ידי מכונות, במיוחד כאשר עובדים על יישומים הדורשים החלפת נתונים באינטרנט או הגדרות תצורה.

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
