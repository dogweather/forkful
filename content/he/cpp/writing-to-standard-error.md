---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה ל-Standard Error (stderr) משמשת לדיווח על שגיאות בתוך תכנית. זה מאפשר לנו להפריד בין פלט רגיל לבין הודעות שגיאה, כך שאפשר לנתב אותן למקומות שונים ולטפל בהן בנפרד.

## איך לעשות:
```C++
#include <iostream>

int main() {
    std::cerr << "שגיאה: הפעולה לא נתמכת" << std::endl;
    return 1;
}
```
פלט לדוגמא:
```
שגיאה: הפעולה לא נתמכת
```

## טבילה עמוקה
`std::cerr` הוא אובייקט גלובלי שמקושר ברירת מחדל ל-Standard Error. בשנות ה-60 וה-70, כאשר UNIX פותח, הבחנה בין Standard Output (stdout) ל-Standard Error הניבה יתרון בטיפול בפלט ושגיאות. תחליפים ל-`std::cerr` כוללים כתיבה לקובץ לוג או שימוש בספריות ניתוח שגיאות. בפנימיות, `std::cerr` אינו מבצע חסימות כמו `std::cout`, הדבר מקטין את העיכוב בהעברת שגיאות.

## ראה גם
- [cppreference.com: std::cerr](https://en.cppreference.com/w/cpp/io/cerr)
- [C++ Super-FAQ: The Standard Streams](https://isocpp.org/wiki/faq/input-output#iostreams-and-stdio)
- [GNU: Standard Streams](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)