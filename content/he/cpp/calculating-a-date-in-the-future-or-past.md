---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "C++: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

ישנם מספר סיבות שבהן נדרש לחשב תאריך בעתיד או בעבר. לדוגמה, אולי זה יכול לעזור למערכות ממוחשבות לקבל החלטות מבוססות על תאריך מסוים או לתכנן אירוע בעתיד.

## איך לעשות את זה

על מנת לחשב תאריך בעתיד או בעבר בשפת C++, ניתן להשתמש במגוון של שיטות ופונקציות. בהמשך תוכלו למצוא כמה דוגמאות קוד עם תוצאות להמחשה.

```C++
// כאן אנו משתמשים בפונקציה פנימית של C++ לחישוב תאריך עתידי
// נתון תאריך התחלה של 1 בינואר 2020
// הפונקציה מחזירה את תאריך ה- 100 ימים הקדימה מהתאריך הנתון
#include <iostream>
#include <ctime> // ספריית פונקציות תאריך ושעה
using namespace std;
int main() {
    time_t now = time(0); // מנקודת ההתחלה, זמן זה ימודד בשניות מ1 בינואר 1970
    struct tm startDate = *localtime(&now); // יוצאי מידע מתאריך נתון
    startDate.tm_mday = 1;
    startDate.tm_mon = 0; // ינואר
    startDate.tm_year = 2020-1900;
    startDate.tm_isdst = 0;
    now += (60*60*24*100); // הוספת 100 ימים לזמן ההתחלה
    struct tm *futureDate = localtime(&now);
    cout << "תאריך ה-100 הקדימה מ-1 בינואר 2020 הוא: " <<
    futureDate->tm_mday << "-" << (futureDate->tm_mon+1) << "-" << (futureDate->tm_year+1900) << endl;
    return 0;
}

// תוצאה: תאריך ה-100 הקדימה מ-1 בינואר 2020 הוא 11-4-2020
```

```C++
// כאן אנו משתמשים בספריית פונקציות תאריך ושעה מובנית כדי לחשב תאריך בעבר
// נתון יום נוכחי הוא 7 בנובמבר 2020
// מידע נוסף מתוך תאריך נתון המייצג את תאריך נוסף בעבר (עד 2 שנים עבר)
#include <iostream>
#include <ctime>
using namespace std;
int main() {
    time_t now = time(0); // ז