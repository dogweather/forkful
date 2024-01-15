---
title:                "קבלת התאריך הנוכחי"
html_title:           "C++: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# למה
קבלת תאריך נוכחי מכילה מידע נוסף שיכול לסייע ביישום התוכנית ובתחזוקתה, בנוסף לתכנות של פעולות זמן-תאריך.

## כיצד לעשות זאת
```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
  // קבלת התאריך והשעה הנוכחיים 
  time_t now = time(0);
  // המרה לפורמט תאריך סטנדרטי
  char* dt = ctime(&now);
  cout << "תאריך ושעה נוכחיים הם: " << dt << endl;
  // המרה למבנה תאריך יפה יותר
  tm *ltm = localtime(&now);
  // הדפסת תיאור מודד שנה
  cout << "השנה היא: " << 1900 + ltm->tm_year << endl;
  // הדפסת תיאור מודד חודש
  cout << "החודש הוא: " << 1 + ltm->tm_mon << endl;
  // הדפסת תיאור נייד
  cout << "היום הוא: " << ltm->tm_mday << endl;
  return 0;
}
```
### פלט:
תאריך ושעה נוכחיים הם: Mon Jul 26 14:21:31 2021
השנה היא: 2021
החודש הוא: 7
היום הוא: 26

## חקירה מעמיקה
כאשר אתה קורא תאריך נוכחי, התוכנית מכילה מידע נוסף כמו רשימת ימי החודש, מספר השנה, חודש עבריים ועוד. בנוסף, תאריך נוכחי מכיל גם פרטים נוספים כגון זמן מוסף כגון חלוקה לשניות ודקות. הפונקציות לקבלת תאריך נוכחי כוללות גם אפשרויות לתאריכי UTC ותאריך בתקופה ספציפית.

# ראה גם
- [פונקציות תאריך- חודש הוסף](https://www.programmingsimplified.com/c/date_time_functions)
- [מדריך לפונקציות תאריך C++](https://www.freecodecamp.org/news/how-to-work-with-dates-in-c-rdate-date-time-tutorial-beginner/)
- [קבלת פרטים נוספים על תאריך נוכחי ושעה](https://www.techiedelight.com/check-current-date-time-cpp/)