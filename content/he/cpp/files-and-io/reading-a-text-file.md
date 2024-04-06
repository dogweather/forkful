---
date: 2024-01-20 17:54:26.325542-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E7\u05D5\u05D3\
  \ \u05DC\u05D3\u05D5\u05D2\u05DE\u05D4."
lastmod: '2024-04-05T21:53:40.927475-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

## איך לעשות:
קוד לדוגמה:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::string line;
    std::ifstream myfile("example.txt"); // שם הקובץ שלך כאן

    if (myfile.is_open()) {
        while (std::getline(myfile, line)) {
            std::cout << line << '\n';
        }
        myfile.close();
    } else {
        std::cout << "Unable to open file";
    }

    return 0;
}
```

פלט לדוגמה:
```
השורה הראשונה של הקובץ
השורה השנייה של הקובץ
...
```

## צלילה לעומק
בשנות ה-70 וה-80, כשתקני ה-C++ עוד לא היו קיימים, קריאת קבצים הייתה תלוית פלטפורמה ויותר מסובכת. עם השנים, הספרייה הסטנדרטית הוסיפה כלים כמו `<fstream>` לקריאה וכתיבה קלה יותר.

אלטרנטיבות: בפרויקטים מודרניים, אנשים לפעמים משתמשים בספריות חיצוניות כמו Boost.Iostreams לטיפול יותר מתקדם בקבצים.

פרטים טכניים: קריאת קבצים יכולה להיות בולמת תהליכים אם הקובץ גדול, לכן לפעמים קוראים אותו באופן אסינכרוני או במנות קטנות.

## ראה גם
- [cplusplus.com - Input/output with files](http://www.cplusplus.com/doc/tutorial/files/)
- [cppreference.com - std::ifstream](https://en.cppreference.com/w/cpp/io/basic_ifstream)
- [Boost IOStreams Library](https://www.boost.org/doc/libs/1_75_0/libs/iostreams/doc/index.html)
