---
title:                "C++: להמיר תאריך למחרוזת"
simple_title:         "להמיר תאריך למחרוזת"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה 
כתיבת קוד ב-C++ מכילה המון תהליכים ופעולות שונות, ולעולם אין "נכון" אחד שיספק את כל הפתרונות לבעיות מסוימות. לכן, כעת אנו נתמקד במטרה אחת בטוחה אשר מתבקשת לפעול על ידי המתכנתים והמתכנתיות האנגלו-סקסוניים - coverting מועיל לכתוב דימוי של תאריך כאשר הם רוצים להתחבר לכתובת URL המודפוסות באופן נחמד.

## איך לעשות זאת
```c++
#include <iostream>
#include <string>
#include <ctime>
using namespace std;

// פונקצית הממירה את התאריך למחרוזת
string convertDateToString(int day, int month, int year)
{
    char formatted_date[15];
    sprintf(formatted_date, "%02d-%02d-%04d", month, day, year); // יצירת מחרוזת באמצעות sprintf בצורה מתאימה לתאריך
    string date_string(formatted_date);
    return date_string;
}

int main()
{
    // דוגמא לפלט של התאריך הממומר למחרוזת בפורמט דדד-אדד-׳יי
    int day = 6;
    int month = 11;
    int year = 2021;
    string date_string = convertDateToString(day, month, year);
    cout << "Date: " << date_string << endl;
    
    return 0;
}
```
כאן, אנו משתמשים בפונקציית sprintf כדי להמיר את התאריך למחרוזת, ולאחר מכן אנו מחזירים את המחרוזת כערך חזרה לפונקציה הראשית. המחרוזת מוצגת בפורמט דדד-מממ-׳שש, שהוא פורמט נפוץ לטיפוח אינטרנטי וסיפק תכניות מסוימות.

## חקירת מעמקים
המרת תאריך למחרוזת היא משימה נפוצה בקוד המקור שלנו. במאמר זה, אנו למדנו כיצד לבצע זאת באמצעות פונקציות ופקודות שונות ב-C++ כדי ליצור תאריך בפורמט נכון. יש סימניות מיוחדות שניתן להוסיף כדי לשנות את הפורמט לתאריך, כך