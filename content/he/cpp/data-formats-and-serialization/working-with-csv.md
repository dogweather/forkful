---
title:                "עובדים עם CSV"
aliases: - /he/cpp/working-with-csv.md
date:                  2024-02-03T19:19:43.980140-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV (Comma Separated Values - ערכים מופרדים בפסיק) עוסקת בעיבוד ובניהול של נתונים המאוחסנים בפורמט טקסט פשוט, שבו כל שורה בטקסט מייצגת שורה בטבלה, ופסיקים מפרידים בין העמודות השונות. מתכנתים משתמשים בכך כדי לייבא, לייצא, ולנהל נתונים בין מערכות שונות, עקב הקבלתה הרחבה של CSV כפורמט החלפת נתונים קל משקל וקריא לאדם.

## איך ל:

### קריאת קובץ CSV באמצעות ספריית התקן של C++:

```cpp
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("data.csv");
    std::string line;
    
    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::string cell;
        std::vector<std::string> parsedRow;
        
        while (std::getline(lineStream, cell, ',')) {
            parsedRow.push_back(cell);
        }
        
        // ניתן לעבד את parsedRow כאן
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### כתיבה לקובץ CSV:

```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Name", "Age", "City"},
        {"John Doe", "29", "New York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for (const auto& row : data) {
        for (size_t i = 0; i < row.size(); i++) {
            file << row[i];
            if (i < row.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### שימוש בספרייה צד שלישי: `csv2`:

למרות שספריית התקן של C++ מספקת את הכלים הבסיסיים לעבודה עם קבצים ומחרוזות, השימוש בספריות של צד שלישי יכול לפשט את עיבוד ה-CSV. ספרייה אחת כזו היא `csv2`, המתוארת בזכות נוחות השימוש והיעילות שלה.

- התקנה: לרוב מותקנת דרך מנהלי חבילות כמו Conan או ישירות מהמחסן שלה ב-GitHub.

דוגמא לשימוש ב-`csv2` לקריאת קובץ CSV:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // הדפסת ערך כל תא
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

תוצאה דוגמא לפעולות קריאה עשויה להיראות כך (בהנחה שמדובר בקובץ CSV פשוט בעל שלוש עמודות):

```
John    29    New York    
Jane    34    Los Angeles
```

הדוגמאות האלו נועדו לכסות את הפעולות הבסיסיות בעבודה עם CSV ב-C++. לסיטואציות מורכבות יותר, כמו עיבוד קבצים גדולים או שינויים מורכבים בנתונים, עשויה להיות צורך בחקירה נוספת לתוך ספריות או כלים מתמחים.
