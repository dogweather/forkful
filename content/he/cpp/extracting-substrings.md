---
title:                "C++: יצירת תת-מחרוזות"
simple_title:         "יצירת תת-מחרוזות"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# למה
במאמר זה נלמד כיצד להוציא תת-מחרוזת מתוך מחרוזת נתונה בשפת סי++.

## כיצד לעשות זאת
לפניכם כמה דוגמאות של קוד סי++ עם פלט תוצאה מצורף. נשתמש בפונקציית substr כדי להוציא תת-מחרוזת לפי אינדקס התחלתי וספירה של אורך התוכן הרצוי.

```C++
// דוגמא 1: הוצאת תת-מחרוזת מתנאי
#include <iostream>
#include <string>

using namespace std;

int main() {
    // מחרוזת נתונה
    string name = "דניס ריצ'י";
    
    // הוצאת תת-מחרוזת מתחילת השם
    string firstName = name.substr(0, 5);
    cout << "שם פרטי: " << firstName << endl;
    
    // הוצאת תת-מחרוזת מהאות האחרונה
    string lastName = name.substr(6);
    cout << "שם משפחה: " << lastName << endl;
    
    return 0;
}
```

פלט תוצאה:

```
שם פרטי: דניס
שם משפחה: ריצ'י
```

```C++
// דוגמא 2: הוצאת תת-מחרוזת בצורה תנאי
#include <iostream>
#include <string>

using namespace std;

int main() {
    // מחרוזת נתונה
    string sentence = "היי, אני לומד סי++";
    
    // הוצאת תת-מחרוזת עבור מחרוזת 'לומד'
    string verb = sentence.substr(6, 8);
    cout << "פועל: " << verb << endl;
    
    // הוצאת תת-מחרוזת עבור מחרוזת 'סי++'
    string language = sentence.substr(16);
    cout << "שפה: " << language << endl;
    
    return 0;
}
```

פלט תוצאה:

```
פועל: לומד
שפה: סי++
```

## לחפור עמוק יותר
הפונקצייה substr במקרה זה מקבלת שני פרמטרים: אינדקס התחלתי ואורך התוכן. המחרוזת המקורית מתחילה מהאינדקס הראשון (0) ולכן אם נשתמש באינדקס שלילי או גדול מתוך המחרוזת, אנחנו נקבל תוצאה ריקה. לדוגמא:

```C++
string sentence = "אני חושב שאני צריך משהו לאכול";
string empty = sentence.substr(-4, 5); // יחזיר