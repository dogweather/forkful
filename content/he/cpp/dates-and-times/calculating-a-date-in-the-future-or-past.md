---
date: 2024-01-20 17:31:30.899547-07:00
description: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\
  \u05E2\u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D4\u05D5\u05D0\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05DC\
  \u05D3\u05E2\u05EA \u05DE\u05D4 \u05D9\u05D4\u05D9\u05D4 \u05D4\u05EA\u05D0\u05E8\
  \u05D9\u05DA \u05DC\u05D0\u05D7\u05E8 \u05DE\u05E1\u05E4\u05E8 \u05DE\u05E1\u05D5\
  \u05D9\u05DD \u05E9\u05DC \u05D9\u05DE\u05D9\u05DD \u05D0\u05D5 \u05DC\u05E4\u05E0\
  \u05D9\u05D5. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DC\u05EA\u05DB\u05E0\u05D5\u05DF \u05D0\u05D9\u05E8\
  \u05D5\u05E2\u05D9\u05DD, \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05D6\
  \u05DE\u05DF, \u05D5\u05DC\u05E7\u05D1\u05DC\u05EA\u2026"
lastmod: 2024-02-19 22:04:59.122220
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D4\u05D5\u05D0 \u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05DC\u05D3\u05E2\
  \u05EA \u05DE\u05D4 \u05D9\u05D4\u05D9\u05D4 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05DC\u05D0\u05D7\u05E8 \u05DE\u05E1\u05E4\u05E8 \u05DE\u05E1\u05D5\u05D9\u05DD\
  \ \u05E9\u05DC \u05D9\u05DE\u05D9\u05DD \u05D0\u05D5 \u05DC\u05E4\u05E0\u05D9\u05D5\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DC\u05EA\u05DB\u05E0\u05D5\u05DF \u05D0\u05D9\u05E8\u05D5\u05E2\
  \u05D9\u05DD, \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05D6\u05DE\u05DF\
  , \u05D5\u05DC\u05E7\u05D1\u05DC\u05EA\u2026"
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פונקציונליות לדעת מה יהיה התאריך לאחר מספר מסוים של ימים או לפניו. מתכנתים עושים זאת לתכנון אירועים, פונקציות זמן, ולקבלת נתונים היסטוריים.

## איך עושים את זה:
השתמש ב-C++20 ובספרייה `<chrono>` לחישובי תאריכים:

```C++
#include <iostream>
#include <chrono>
#include <date/date.h>  // אם יש צורך בתוספת של Howard Hinnant's date library

int main() {
    using namespace std::chrono;
    using namespace date;

    // היום
    auto today = floor<days>(system_clock::now());
    
    // הוסף 30 ימים להיום
    auto future_date = today + days{30};
    
    // הורד 30 ימים מהיום
    auto past_date = today - days{30};

    // הדפסה למען ידע
    std::cout << "היום: " << today << "\n";
    std::cout << "תאריך עתידי: " << future_date << "\n";
    std::cout << "תאריך עברי: " << past_date << "\n";
    
    return 0;
}
```

פלט לדוגמא:
```
היום: 2023-03-29
תאריך עתידי: 2023-04-28
תאריך עברי: 2023-02-27
```

## צלילה לעומק:
עד ל-C++20, התקנים לא סיפקו תמיכה רחבה לניהול תאריכים וזמנים. מערכת ה-`<chrono>` הורחבה ב־C++20 כדי לכלול יותר פונקציות נוחות לעבודה עם תאריכים ושעות. לפני כן, היינו צריכים להשתמש בספריות חיצוניות כמו `boost` או `date` של Howard Hinnant. אפשרויות נוספות כוללות ספריית `tm` של C, אבל היא קטנה יותר וייתכן שפחות בטוחה. דיקוי תאריכים מתבצע גם בדרך של חישוב ימים עסקיים (לדוגמה, לא סופרים סופ"ש).

## לקרוא גם:
- [הספריית `<chrono>`](https://en.cppreference.com/w/cpp/header/chrono)
- [מסמכים לספריית `date` של Howard Hinnant](https://github.com/HowardHinnant/date)
- [מדריך למערכת הזמנים של C++20](https://cplusplus.com/reference/chrono/)
