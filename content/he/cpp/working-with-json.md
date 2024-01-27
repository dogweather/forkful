---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
JSON זוהי תצורת נתונים בפורמט טקסטואלי, פשוטה ונפוצה לשימוש. תכנתים משתמשים בה כדי לחלוק נתונים בין שרתים ולקוחות, ולשמור תצורות קומפלקסיות בצורה קריאה וגמישה.

## איך לעשות:
השתמש בנתוני JSON ב-C++ דורש ספרייה נוספת. נפוץ להשתמש ב-`nlohmann/json`.

```C++
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // יצירת אובייקט JSON פשוט
    nlohmann::json my_json;
    my_json["שם"] = "יונתן";
    my_json["גיל"] = 30;
    my_json["תחביבים"] = {"קריאה", "הליכה", "משחקי מחשב"};

    // הדפסת ה-JSON לפלט
    std::cout << my_json.dump(4) << std::endl;

    // Parsing JSON ממחרוזת
    std::string json_str = R"({"עיר": "תל אביב", "אוכלוסיה": 451523})";
    nlohmann::json parsed_json = nlohmann::json::parse(json_str);

    // הדפסת ערכים מה-JSON המפוענח
    std::cout << "עיר: " << parsed_json["עיר"] << ", אוכלוסיה: " << parsed_json["אוכלוסיה"] << std::endl;

    return 0;
}
```

תוצאת הדוגמה:
```plaintext
{
    "גיל": 30,
    "שם": "יונתן",
    "תחביבים": [
        "קריאה",
        "הליכה",
        "משחקי מחשב"
    ]
}
עיר: תל אביב, אוכלוסיה: 451523
```

## צלילה עמוקה
JSON (JavaScript Object Notation) הוא פורמט התקני שהתפתח מתוך שפת התכנות JavaScript אבל הוא נתמך היום על ידי רוב השפות. תחליפים כמו XML קיימים, אבל JSON נחשב יותר קל לשימוש. על מנת לעבוד עם JSON ב-C++, יש להשתמש בספריות חיצוניות מכיוון שהשפה לא מציעה תמיכה טבעית בפורמט זה. `nlohmann/json` היא אחת הספריות הפופולאריות לטיפול ב-JSON ב-C++, המאופיינת בממשק קל ונוח לשימוש.

## ראו גם
- מסמכי התיעוד הרשמיים של ספריית `nlohmann/json`: https://github.com/nlohmann/json
- מדריך ל-JSON על MDN: https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON
- סקירה עמוקה יותר של פורמט ה-JSON: https://www.json.org/json-en.html
