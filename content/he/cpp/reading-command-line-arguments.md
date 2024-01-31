---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:55:35.825615-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים מקו הפקודה מאפשרת לתוכנית לקבל נתונים מחוץ לה בזמן ריצה. תכניתנים עושים זאת כדי להפוך את התוכנות שלהם לגמישות ואינטרקטיביות יותר.

## איך לעשות:
```C++
#include <iostream>

int main(int argc, char *argv[]) {
    std::cout << "יש לך " << argc << " ארגומנטים:\n";
    for (int i = 0; i < argc; ++i) {
        std::cout << i << ": " << argv[i] << '\n';
    }
    return 0;
}
```
הפעלה: `./your_program שלום עולם`
פלט:
```
יש לך 3 ארגומנטים:
0: ./your_program
1: שלום
2: עולם
```

## צלילה עמוקה:
ב-C++, ארגומנטים מקו הפקודה נקראים בזמן התחלת התוכנית דרך פונקציית ה-main. היסטורית, השימוש ב-argc (argument count) ו-argv (argument vector) מקורו בתכנות יוניקס. אלטרנטיבות כוללות שימוש במחלקות כגון `std::vector` ו`std::string` לניהול נוח יותר של ארגומנטים. פרטי המימוש כוללים המרה של ארגומנטים גולמיים לסוגים מתוחזקים יותר בתוך התוכנה.

## ראה גם:
- [cppreference Command line arguments](https://en.cppreference.com/w/cpp/language/main_function)
- [GNU: Program Argument Syntax Conventions](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)
- [Stack Overflow: How can I parse a comma-separated list of command line arguments in C++?](https://stackoverflow.com/questions/1894886/parsing-a-comma-delimited-stdstring)
