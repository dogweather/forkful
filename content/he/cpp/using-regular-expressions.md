---
title:    "C++: שימוש בביטויים רגולריים"
keywords: ["C++"]
---

{{< edit_this_page >}}

## לכל מה? 

מצב התקווה שלכולם כולל התנסות עם ספריות תוויות רגולריות. באמצעותן ניתן לחפש, למצא ולהחליף חלקים של טקסט בקלות ומהירות. בעזרת תוויות רגולריות אפשר לחפש בקיפוד ולקבל תוצאות מדויקות מאוד.

## כיצד להשתמש?

עבור הדוגמה הבאה, נניח שאנחנו רוצים למצוא ולקבל רשימת כל המספרים המקומיים משורה של קובץ טקסט.

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    string text = "המספר הראשון: 054-123-4567, המספר השני: 052-987-6543";
    regex pattern("(05[0-9]{2}(-)?)[0-9]{3}(-)?[0-9]{4}");
    smatch matches;
    
    while (regex_search(text, matches, pattern)) {
        // הטקסט שמתאים לתוויות רגולריות
        cout << matches[0] << endl;
        
        // מיידע נוסף על התוצאה
        cout << matches[1] << endl; // מספר האיזור
        cout << matches[2] << endl; // סימן מינוס אם קיים
        cout << matches[3] << endl; // סימן מינוס אם קיים
        
        // מעביר לטקסט הבא בשורה
        text = matches.suffix().str();
    }
    
    return 0;
}
```

*פלט:*

```
054-123-4567
054
-
-
052-987-6543
052
-
```

בדוגמה זו, אנו משתמשים בתוויות רגולריות כדי לחפש מספרים מהמבנה מסוים. התוויות מוגדרות ב- `regex` ושמורות במשתנה בשם `pattern`. באמצעות הפונקציה `regex_search` אנו מתאימים את התוכנית לטקסט ומעבירים את התאמה האחרונה בכל התוצאות. ניתן לגשת למידע נוסף על תוצאת הקוד באמצעות שימוש במערך התאמות `smatch`. מידע מדויק נוסף יכול להיות ניתן לאיזור כגון מספר איזור או סימן מינוסים. לבסוף, אנו מרח