---
title:                "עובדים עם קובץ csv"
html_title:           "C++: עובדים עם קובץ csv"
simple_title:         "עובדים עם קובץ csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## מדוע

פורמט CSV הוא צורת קובץ נפוץ ונוח לעיבוד נתונים בקוד. לא קשה לקרוא CSV ולהעביר את הנתונים לטיפוסים ידידותיים יותר כגון מחרוזות או מספרים מחרוזות.

## כיצד לעבוד עם CSV

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

int main() {
  // קורא קובץ CSV לתוך משתנה
  ifstream file("data.csv");
  // משתנה לקליטת שורות מהקובץ
  string line;
  // מציג את כל הנתונים בפורמט שורה-עמודה
  while (getline(file, line)) {
    // כיוון ששורה של CSV מופרדת על ידי פסיק, אנחנו משתמשים בפסיק כדי לחלק את השורה למספר עמודות
    vector<string> data;
    string column = "";
    for (int i = 0; i < line.size(); i++) {
      // אם מדובר בפסיק, מוסיף את המידע שנמצא בקולונה לווייקטור ומתאפס את המשתנה
      if (line[i] == ',') {
        data.push_back(column);
        column = "";
      }
      else {
        // אם לא מדובר בפסיק, ממשיך להוסיף את התווים לתוך הקולונה
        column += line[i];
      }
    }
    // מוסיף את הנתונים לווייקטור הכללי של הנתונים
    data.push_back(column);
    // העברת נתונים לטיפוסים ידידותיים יותר לעיבוד נוסף
    string name = data[0];
    int age = stoi(data[1]);
    double height = stod(data[2]);
    // מדפיס את הנתונים בפורמט שרציתם
    cout << "Name: " << name << ", Age: " << age << ", Height: " << height << endl;
  }
  return 0;
}
```

הפלט של הקוד הינו:
```console
Name: John, Age: 25, Height: 1.75
Name: Sarah, Age: 30, Height: 1.65
Name: David, Age: 40, Height: 1.8
```

## מעמקים

כאשר מעבדים CSV, חשוב לזכור שקבצי CSV לא תמיד מתאימים למבנה סטיים של C++. יתכן שתקבלו תווים מיוחדים או ערכים ריקים שמבנה הסטיים לא יודע לטפל בהם. כדאי לבדוק את הנתונ