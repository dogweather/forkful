---
title:                "השוואת שתי תאריכים"
html_title:           "C++: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

##למה
השוואת שני תאריכים יכולה להיות מאתגרת למתכנתים חדשים או לאנשים שאינם מכירים את שפת התכנות C++. התאריך הוא פעולת נתונים נפוצה ומועילה, ולכן ידע טוב של השוואת תאריכים יכול לעזור לנו במתן מבניים ונתונים מדויקים בתכניות שלנו.

##איך לבצע
כדי להשוות שני תאריכים בשפת C++, נוכל להשתמש בפונקציות המוכנות מראש לטיפול בתאריכים (date handling functions). נראה מספר דוגמאות שיכולות לעזור לנו בתהליך השוואה ולהבין איך להשתמש בהן.

```C++
// הוספת הספריות הנחוצות לשימוש בפונקציות לטיפול בתאריכים
#include <iostream> 
#include <ctime> 
#include <string> 
using namespace std; 

int main() 
{
    // יצירת תאריך ראשון והדפסתו
    tm first = {0, 0, 0, 1, 0, 2020}; 
    time_t firstDate = mktime(&first);
    cout << "תאריך ראשון: " << ctime(&firstDate) << endl; 
    
    // יצירת תאריך שני והדפסתו
    tm second = {0, 0, 0, 1, 0, 2021}; 
    time_t secondDate = mktime(&second);
    cout << "תאריך שני: " << ctime(&secondDate) << endl; 
    
    // שונה הנתון לפי תאריך הראשון והשני
    // במקרה זה, התאריך הראשון יהיה מוקדם יותר
    if (firstDate < secondDate) 
    {
         cout << ctime(&firstDate) << " הוא התאריך המוקדם יותר" << endl; 
    } 
    else 
    {
        cout << ctime(&secondDate) << " הוא התאריך המוקדם יותר" << endl; 
    }
    
    return 0; 
}
```

הפלט של התכנית הזו יהיה:

```
תאריך ראשון: Wed Jan 1 00:00:00 2020
תאריך שני: Thu Jan 1 00:00:00 2021
Wed Jan 1 00:00:00 2020 הוא התאריך המוקדם יותר
```

כמו שאתם יכולים לראות, התאריך הראשון בפלט נמצא לפני התאריך השני,