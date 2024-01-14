---
title:    "C++: המרת מחרוזת לאותיות קטנות"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# למה

המרת מחרוזת לאותיות קטנות נחשבת לפעולה נפוצה בתכנות בשפת C++. היא מאפשרת למתכנתים להפוך את כל התווים הגדולים של מחרוזת לתווים קטנים, מה שמאפשר עיבוד נתונים בצורה נוחה יותר.

# איך לעשות זאת

ישנם מספר דרכים לבצע המרת מחרוזת לאותיות קטנות בשפת C++. נביא כמה דוגמאות כדי להדגים את התהליך.

```C++
#include <iostream>
#include <string>
#include <cctype>

using namespace std;

int main() {
    // דוגמא 1: שימוש במתודה transform בקוד STL
    string str = "HELLO WORLD";
    transform(str.begin(), str.end(), str.begin(), ::tolower);
    cout << str << endl; // תוצאה: hello world

    // דוגמא 2: שימוש בלולאה פור כדי לעבור על כל התווים במחרוזת ולהפוך אותם לאותיות קטנות
    string text = "THIS IS A TEST";
    for (int i = 0; i < text.length(); i++) {
        text[i] = tolower(text[i]);
    }
    cout << text << endl; // תוצאה: this is a test

    return 0;
}
```

# חקירה מעמיקה

כדי להבין טוב יותר את תהליך המרת מחרוזת לאותיות קטנות, חשוב להבין את ההבדלים בין אותיות גדולות וקטנות בטבלת התווים האסקי.

בשפת C++, כל אות גדולה וקטנה בקוד אסקי מיוצגת על ידי מספר מסוים. למשל, אות ה "A" לגבי מחשב באסקי מיוצגת כמספר 65, ואות ה "a" מיוצגת כמספר 97. על ידי השוואת המספרים הללו, אנו יכולים להבין את ההבדלים בין אותיות הגדולות והקטנות.

כאשר אנו משתמשים במתודה transform בקוד STL, אנו מעבירים את הפרמטר ::tolower, המכיל את המספר 97. כך, המתודה transform מחליפה כל אות גדולה במחרוזת לתווים קטנים תואמים, בהתאם