---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה & למה?
קריאת קבצי טקסט הינה פעולת השגת המידע המאוחסן בתוך קובץ טקסט והצגתו. מתכנתים עושים את זה כדי לעבד ולנתח מידע, לדוגמא: קריאת הגדרות, נתונים לתכנית, וכו'.

## איך לעשות:
הנה דוגמה של איך לקרוא קובץ טקסט באמצעות C++:

```C++
#include <iostream>
#include <fstream>
#include <cstring>

int main ()
{
    std::ifstream inputFile("example.txt");

    if(inputFile)
    {
        std::string line;
        while(std::getline(inputFile, line))
        {
            std::cout << line << '\n';
        }
    }
    else
    {
        std::cerr << "Failed opening file\n";
    }

    
    inputFile.close();
    return 0;
}
```

## צלילה עמוקה
אפשר לקרוא קבצים ב-C++ על ידי שימוש ב-iostreams, שהוא מנגנון שנוצר מאז מהדורת ANSI C++ לתמיכה בעבודה עם נתונים תחת הגדרות משתמש. חלופה לזה היא פונקציות סטנדרטיות של C, כמו fopen ו-fread. אפשר לבחור את השיטה המתאימה התלויה בתחילה, קוד ייחודי, ופרמטרים ספציפיים אחרים.

## עיין גם 
מסמכי יחס על C++ iostreams והיסטוריה שלהם:
1. [Cppreference - Input/output with files](https://en.cppreference.com/w/cpp/io/c)
2. [Learn Cpp - 27.9 A Brief Introduction to C++ File I/O](https://www.learncpp.com/cpp-tutorial/279-a-brief-introduction-to-c-file-io/)

מסמכים על שיטות קריאת קבצים בשפות אחרות:
1. [Python File Reading](https://docs.python.org/3/tutorial/inputoutput.html)
2. [Java File Reading](https://docs.oracle.com/javase/tutorial/essential/io/file.html)