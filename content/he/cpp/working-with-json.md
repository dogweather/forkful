---
title:                "עבודה עם json"
html_title:           "C++: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-json.md"
---

{{< edit_this_page >}}

# מה ולמה?

עבודה עם JSON היא תהליך שבו מתנהגים עם נתונים בפורמט טקסטואלי. זהו סגנון פופולרי בין מתכנתים עקב יתרוניו ביחס לפורמטים אחרים כמו XML או CSV. מתכנתים עוברים לעבוד עם JSON בגלל נוחיות הפורמט, אפשרות לעבוד עם נתונים מורכבים וזמינות במגוון שפות תכנות.

# איך ל:

### דוגמאות קידוד ופלט

```C++
#include <iostream>
#include <json/json.h>

int main() {

    // יצירת אובייקט עם נתונים
    Json::Value Person;
    Person["name"] = "John";
    Person["age"] = 30;
    Person["married"] = false;

    // קידוד לפורמט JSON
    std::string jsonOutput = Person.toStyledString();

    // הדפסת פלט
    std::cout << jsonOutput << std::endl;

    return 0;
}

```
**Output:**

```json
{
   "name": "John",
   "age": 30,
   "married": false
}
```

### קריאת נתונים מפורמט JSON

ניתן לקרוא נתונים מפורמט JSON באמצעות ספריית `Jsoncpp`. הדוגמה הבאה מדגימה כיצד ניתן לקרוא את הנתונים מהפורמט:

```C++
#include <iostream>
#include <json/json.h>

int main() {

    // קריאת הנתונים מפורמט JSON
    const std::string jsonInput = "{ \"name\": \"John\", \"age\": 30, \"married\": false }";
    Json::Value Person;
    Json::Reader reader;

    bool parsingSuccessful = reader.parse(jsonInput, Person); // קריאה של הנתונים לאובייקט

    // בדיקת המצב
    if (!parsingSuccessful) {
        // טיפול בשגיאות
        std::cout << "Failed to parse the input." << std::endl;
        return 1;
    }

    // הדפסת הנתונים
    std::cout << "Name: " << Person["name"].asString() << std::endl;
    std::cout << "Age: " << Person["age"].asInt() << std::endl;
    std::cout << "Married: " << Person["married"].asBool() << std::endl;

    return 0;
}
```
**Output:**
```
Name: John
Age: 30
Married: false
```

# חקירה מעמיקה:

### היסטוריה

פורמט JSON נוצר בשנת 2001 על ידי דואגלס קרוקפורד, טעם מתכנת מארגן ומפתח סופטוור וכיום נתמך על ידי רוב השפות התכנותיות המודרניות כמו C++, Java ו-Python.

### אלטרנטיבות

פורמט JSON לא נתמך על ידי כל התכנות, וכך מתאים למתכנתים שנהנים מהחזקות שלו. אם מתכנת יותר מעוניין באפשרויות נתונים שלא קיימות בפורמט JSON, פורמט כמו XML יכול להיות אלטרנטיבה נוחה יותר.

### פירוט המימוש

ספריית `Jsoncpp` מממשת את בניית העץ של הנתונים הקיימים בפורמט JSON ואת פונקציונליות קריאת וכתיבת הנתונים. הספרייה כוללת גם אפשרויות נוספות כמו אפשרות לצרוך ולייצא נתונים מהפורמט לפורמטים אחרים כמו CSV או XML.

# ראו גם:

ספריית Jsoncpp: https://github.com/open-source-parsers/jsoncpp