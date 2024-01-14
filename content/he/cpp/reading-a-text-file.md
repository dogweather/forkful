---
title:                "C++: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

##למה
כיוון שקריאת קבצי טקסט הוא חלק חשוב בתכנות ומאפשרת לבדוק ולהשתמש במידע ממקור חיצוני. כתיבת קוד לקריאת קבצי טקסט היא מיומנות חיונית לכל מתכנת.

##כיצד לעשות זאת
```c++
#include <iostream>
#include <fstream>

// פתיחת קובץ טקסט לכתיבה 
std::ifstream file("example.txt");

// בדיקה אם הקובץ נפתח בהצלחה
if(file.is_open()){
    //משתנה לשמירת קלט מהקובץ
    std::string input;
    //לולאה לחיצוניית קריאת הקובץ 
    while(getline(file, input)){
        std::cout << input << std::endl; // תדפיס את התוכן של הקובץ שורה-שורה
    }
    // סגירת קובץ
    file.close();
} 
else{
    // לפעולות אם הקובץ לא נפתח
}
````

Output:
```text
שלום עולם!
היי אני קורא קובץ טקסט.
```

##עומק נוסף
כדי לקרוא קובץ טקסט ב-C++, ניתן להשתמש בעזרתה של הספריה fstream. פקודות חשובות לשימוש כאשר מתרגלים את קריאת קבצי טקסט הן: 
1. `open()` - לפתיחת קובץ
2. `close()` - לסגירת קובץ
3. `getline()` - לקריאת שורה שלמה מהקובץ
4. `eof()` - לבדיקה אם הקובץ הגיע לסופו
מומלץ גם לקרוא על כיצד לנהל שגיאות כאשר מתבצעת קריאה לקובץ טקסט.

##ראה גם
- [מדריך לאיתור וטיפול בשגיאות קריאת קבצי טקסט ב-C++](https://www.geeksforgeeks.org/problems-with-reading-writing-a-text-file-in-c-c/)
- [ספריה fstream ב-C++](http://www.cplusplus.com/reference/fstream/)
- [קריאה לקובץ טקסט עם רווחים ב-C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)