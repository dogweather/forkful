---
title:                "C++: קבלת תאריך נוכחי"
simple_title:         "קבלת תאריך נוכחי"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

איך אנחנו יודעים את התאריך הנוכחי? תאריך נוכחי זה אכן חשוב מאוד עבור תוכניות המחשב שלנו. ייתכן שהנך צריך להדפיס את התאריך הנוכחי כחלק מפונקציות של התכנית שלך או כדי ליצור תדירות לאירועים מסוימים ביומן. במאמר זה אנו נלמד איך לקבל את התאריך הנוכחי בתוך תכנית C++ וכיצד ניתן להשתמש בו.

## כיצד לעשות זאת

לפניכם ישנם כמה אפשרויות להשתמש בתאריך הנוכחי בתוך תכנית C++. נדגים כמה דוגמאות באמצעות קוד ותוכלו לראות את התוצאות המתקבלות עבור כל אפשרות.

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main()
{
  // אפשרות 1: משתמשים בפונקציית זמן נוכחית לקבלת התאריך הנוכחי
  time_t now = time(0);
  tm* timeNow = localtime(&now);

  // מדפיס את התאריך הנוכחי כחלק מהודעה
  cout << "התאריך הנוכחי הוא: " << timeNow->tm_mday << "/" << (timeNow->tm_mon + 1) << "/" << (timeNow->tm_year + 1900) << endl;

  // אפשרות 2: משתמשים בפונקציית זמן אפשרית יותר לקבלת התאריך הנוכחי
  time_t now = time(0);
  char* timeNow = ctime(&now);

  // מדפיס את התאריך הנוכחי כחלק מהודעה
  cout << "התאריך הנוכחי הוא: " << timeNow << endl;

  // אפשרות 3: משתמשים במחלקת תאריך ושעה של C++11 לקבלת התאריך הנוכחי
  #include <chrono> 
  #include <ctime> 

  // מחשב את התאריך הנוכחי בתוך משתנה
  auto now = std::chrono::system_clock::now(); 
  time_t now_c = std::chrono::system_clock::to_time_t(now); 

  // מדפיס את התאריך הנוכחי כחלק מהודעה
  cout << "התאריך הנוכחי הוא: " << ctime(&now_c) << endl;

  return 0;
}
```

כאן אנו משתמשים במספר א