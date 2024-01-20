---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "C++: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

### מה ולמה?
בדיקה אם ספרייה קיימת מהווה, פשוט פשוט, חיפוש לוֹקציה מסוימת במערכת ההפעלה שלכם. מתכנתים בודקים זאת כדי למנוע שגיאות שעלולות להתרחש כאשר ניסיון לגשת לספרייה שלא קיימת או ליצור ספרייה שנמצאת כבר שם.

### איך לבצע:
במטה בקוד ה- C++, אתה יכול לראות כיצד לבדוק אם ספרייה קיימת או לא.
```C++
#include <sys/stat.h>
#include <iostream>

bool doesDirectoryExist(const std::string& dirName_in) {
    struct stat info;
    if(stat(dirName_in.c_str(), &info) != 0) {
        return false;
    } else if(info.st_mode & S_IFDIR) {
        return true;
    }
    return false;
} 

int main() {
    std::string dirName = "/מקום/ל/ה/ספרייה/שלך";
    if (doesDirectoryExist(dirName))
        std::cout << dirName << " exist.\n";
    else
        std::cout << dirName << " does not exist.\n";
    return 0;
}
```
במקרה זה, אנחנו משתמשים בפונקציה של שימוש בנתונים המבנית stat, שמאחסנת מידע אודות הספרייה.

### צלילה עמוקה:
בעבר, שֵׂמִיתִים משתמשים בסילוקים כדי לחפש ספריות. אבל זה יכול להיות בעייתי במערכת הפעלה ייחודית מאוד, שלא מראה את המילים המתאימות. אם לא תמצא ספרייה, C++ ייתן שגיאה. לכן, אנחנו משתמשים ב-fstat במקום.

אפשרות אחרת היא השימוש ב- boost::filesystem. שימוש ב- boost::filesystem::exists מספקת דרך נוחה ונוחה לבדוק אם ספרייה קיימת.

זהות הספרייה היא בהחלט חשובה במהלך הבדיקה. זה שם הספרייה שתבדוק. אם הספרייה לא תמצא, התוכנית תחזיר שגיאה.

### ראה גם:
ליישום הפעולה שלך, אתה יכול לעיין במקורות הבאים:
- [מדריך למתכנתים של C++](https://www.learncpp.com/)
- [Stack Overflow: כיצד לבדוק אם הספרייה קיימת](https://stackoverflow.com/questions/8233842/how-to-check-if-directory-exist-using-c-and-winapi)
- [Boost Filesystem Library](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)
- [מקורות ספריית לינוקס](https://linux.die.net/man/2/stat)