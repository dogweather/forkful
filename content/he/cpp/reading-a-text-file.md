---
title:                "קריאת קובץ טקסט"
date:                  2024-01-20T17:54:26.325542-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט ב-C++ היא פעולה בסיסית שבה אנחנו מנגנים מידע מתוך קובץ כדי לעבד אותו. מתכנתים עושים את זה בדרך כלל כדי לטעון קונפיגורציות, נתונים או סקריפטים לתוך התוכנית.

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
