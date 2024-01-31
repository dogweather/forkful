---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה בקובץ טקסט ב-C++ זה פשוט לכתוב נתונים בקובץ שאפשר לקרוא בכל עורך טקסט. תכניתנים עושים את זה כדי לשמור נתונים, לוגים או כדי לאפשר שיתוף נתונים בין תוכנות.

## איך לעשות:
קוד פשוט לכתיבה בקובץ:

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    ofstream myfile("example.txt"); // יוצר קובץ 
    if (myfile.is_open()) {
        myfile << "שלום, עולם!" << endl; // כותב לקובץ
        myfile.close(); // סוגר את הקובץ
    } else {
        cout << "לא ניתן לפתוח את הקובץ";
    }
    return 0;
}
```

פלט:
```
קובץ example.txt נוצר עם הטקסט "שלום, עולם!" בתוכו.
```

## צלילה לעומק:
היסטורית, תוכניות C++ משתמשות ב-iostreams לכתיבת קבצים. אפשרויות חלופיות כוללות שימוש ב-C APIs כמו `fopen` ו-`fprintf`. השימוש ב-`<fstream>` ב-C++ עכשווי מספק אינטרפייס גבוה יותר והוא שקוף יותר לשימוש.

## ראה גם:
- [cplusplus.com - Input/output with files](http://www.cplusplus.com/doc/tutorial/files/)
- [cppreference.com - File input/output](https://en.cppreference.com/w/cpp/io)
