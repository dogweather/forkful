---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
simple_title:         "עבודה עם קבצי CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
עבודה עם קבצי CSV כוללת קריאה וכתיבה של נתונים בפורמט טקסט שבו הערכים מופרדים בפסיקים. תכניתנים עובדים עם CSV כיוון שמדובר בפורמט פשוט, אוניברסלי וקל לתחזוקה לשיתוף נתונים בין יישומים.

## How to (איך לעשות זאת)
קוד לקריאת CSV:
```C++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

std::vector<std::vector<std::string>> readCSV(const std::string& filename) {
    std::vector<std::vector<std::string>> data;
    std::ifstream file(filename);
    
    std::string line;
    while(std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::string cell;
        std::vector<std::string> row;
        
        while(std::getline(lineStream, cell, ',')) {
            row.push_back(cell);
        }
        
        data.push_back(row);
    }
    
    return data;
}

int main() {
    auto data = readCSV("example.csv");
    
    for (const auto& row : data) {
        for (const auto& cell : row) {
            std::cout << cell << " ";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

פלט דוגמה:
```
name age
Alice 30
Bob 25
```

קוד לכתיבת CSV:
```C++
#include <iostream>
#include <fstream>
#include <vector>

void writeCSV(const std::string& filename, const std::vector<std::vector<std::string>>& data) {
    std::ofstream file(filename);
    
    for (const auto& row : data) {
        for (size_t i = 0; i < row.size(); ++i) {
            file << row[i];
            if (i < row.size() - 1) file << ",";
        }
        file << std::endl;
    }
}

int main() {
    std::vector<std::vector<std::string>> data = {
        {"name", "age"},
        {"Alice", "30"},
        {"Bob", "25"}
    };
    
    writeCSV("example.csv", data);
    
    std::cout << "CSV file written successfully." << std::endl;
    
    return 0;
}
```

פלט דוגמה:
```
CSV file written successfully.
```

## Deep Dive (לעומק)
פורמט CSV נעשה בשימוש רחב כבר משנות ה-70. חלופות נפוצות כוללות JSON ו-XML. בעת עבודה עם CSV חשוב לטפל באתגרים כמו שדות עם פסיקים, ציטוטים או שורות חדשות כחלק מהנתונים.

## See Also (ראו גם)
- מדריך לסיסמא C++ CSV Parser: https://github.com/ben-strasser/fast-cpp-csv-parser
- הגדרה ותקנים של CSV מ-MIME: https://tools.ietf.org/html/rfc4180
- תיעוד לספרייה הסטנדרטית של C++: http://www.cplusplus.com/reference/

זכרו, הקוד הנ"ל הוא מודל בסיסי. למיזמים רציניים, שקלו שימוש בספריות מתקדמות שטופלו בהם גם מקרי קצה.
