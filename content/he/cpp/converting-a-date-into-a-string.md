---
title:                "C++: המרת תאריך למחרוזת"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

כדי להציג תאריך בפורמט שנוח יותר לקריאה ושימוש בתוכניות, יש צורך להמיר את התאריך למחרוזת.

## איך לעשות זאת

תהליך המרה פשוט יחסית: יש להשתמש בפונקציית `strftime()` ולציין את התבנית המתאימה לפורמט תאריך ואת המשתנים הרלוונטים. למשל:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    //קביעת זמן נוכחי
    time_t currentTime = time(0);

    //המרת התאריך למחרוזת
    string dateString = ctime(&currentTime);

    cout << "התאריך הנוכחי הוא: " << dateString << endl;

    return 0;
}
```

פלט:

```bash
התאריך הנוכחי הוא: Mon Oct 25 21:50:40 2021
```

כמו בדוגמה המוצגת, המרת התאריך תציג את התאריך בפורמט הכללי של מחרוזת תאריך ושעה. אפשר להוסיף את `#include <iomanip>` כדי להתאים את המתאריך לפורמט חלק יותר.

## חפירה עמוקה

כדאי לקחת בחשבון שפונקציית `strftime()` יכולה להימצא רק עבור מספר סיסמאות קבועות ומוגבלות. אם יש צורך בתבנית מיוחדת או בערך רלוונטי כמו פורמט של תאריך מותאם אישית, אפשר להשתמש במספר אפשרויות נוספות כמו `stringstream` ו- `put_time()`.

## ראו גם

- [פונקציית strftime ב-C++](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [עבודה עם תאריכים ב-C++](https://www.programiz.com/cpp-programming/library-function/ctime)
- [תצוגת תאריך מותאם אישית ב-C++](https://www.geeksforgeeks.org/strftime-function-in-c-with-examples/)