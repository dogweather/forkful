---
title:                "קריאת קובץ טקסט"
html_title:           "C++: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מדוע

בכדי לעבוד בקוד שכתבנו בעצמנו והרצה עליו, נצטרך לקרוא כתוביות שונות תחת קובץ עם סיומת .ttxt או .txt כדי לאחסן מידע אחר.

## איך לקרוא בפאייתון

```C++
#include <fstream>
#include <iostream>
using namespace std;

int main() {
    string line;
    ifstream file("example.txt"); // replace with your file path
    if (file.is_open()) {
        while (getline(file, line)) {
            cout << line << endl;
        }
        file.close();
    }
    return 0;
}
```

כאן אנו משתמשים בספריית `<fstream>` לקריאת הקובץ וסטרינג לקרוא כל שורה מהקובץ. נפתח את הקובץ באמצעות הפקודה `ifstream` ונותנים לקובץ שם. אחר כך, נבדוק אם הפתיחה נעשתה בהצלחה ואם כן, נעבור על כל שורה עם הפקודה `getline` ונדפיס אותה. סופית, נסגור את הקובץ עם הפקודה `close`.

## פינה עמוקה

כאשר אנו קוראים קובץ טקסט, אנו מעבירים נקודת קלט על כל שורה ונחלצים מידע נדרש. ניתן לשים לב כי אנו משתמשים בלולאת `while` כדי לעבור על כל הקובץ, אך ניתן גם להשתמש בלולאת `for` עם מספר מסופק מראש של שורות שאנו רוצים לקרוא.

## ראה גם

- [כיצד לקרוא קבצי טקסט בשפת פייתון](https://www.tutorialspoint.com/python/python_files_io.htm)
- [קריאת קבצים בשפת C++](https://www.cplusplus.com/doc/tutorial/files/)