---
date: 2024-01-20 17:55:35.825615-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E4\u05E2\
  \u05DC\u05D4: `./your_program \u05E9\u05DC\u05D5\u05DD \u05E2\u05D5\u05DC\u05DD\
  ` \u05E4\u05DC\u05D8."
lastmod: '2024-04-05T22:37:48.316887-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E4\u05E2\u05DC\u05D4: `./your_program \u05E9\u05DC\u05D5\u05DD\
  \ \u05E2\u05D5\u05DC\u05DD` \u05E4\u05DC\u05D8."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

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
