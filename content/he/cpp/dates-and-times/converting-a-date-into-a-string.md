---
title:                "המרת תאריך למחרוזת"
aliases:
- /he/cpp/converting-a-date-into-a-string/
date:                  2024-01-20T17:36:58.828984-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת היא פעולה שבה משנים את פורמט התאריך לטקסט. תכניתנים עושים זאת כדי להציג תאריכים בצורה קריאה למשתמש או לשמור תאריכים בבסיסי נתונים וקבצים בפורמט סטנדרטי.

## איך לעשות:
ב-C++, ניתן להשתמש בספריית `<chrono>` לטיפול בתאריכים וזמנים ובספריית `<iomanip>` להמרתם למחרוזות.

```C++
#include <iostream>
#include <iomanip>
#include <chrono>
#include <sstream>

int main() {
    // קבלת תאריך נוכחי
    auto now = std::chrono::system_clock::now();
    std::time_t t = std::chrono::system_clock::to_time_t(now);
    std::tm* now_tm = std::localtime(&t);

    // שימוש ב- stringstream כדי ליצור מחרוזת עם התאריך
    std::stringstream ss;
    ss << std::put_time(now_tm, "%d-%m-%Y %H:%M:%S");
    std::string date_str = ss.str();

    // הדפסת התאריך כמחרוזת
    std::cout << date_str << std::endl;

    return 0;
}
```

תוצאת דוגמה:
```
04-02-2023 14:55:41
```

## עמק השווה:
בעבר, המרת תאריכים למחרוזת הייתה פחות אינטואיטיבית בגלל שהיה צורך להתעסק עם ctime ופונקציות כמו `strftime`. מאז C++11, ספריית `<chrono>` מאפשרת טיפול נוח ובטוח יותר בזמן. לעומת זאת, ספריית `<fmt>` (שתוכננה להיות חלק מ-C++20) מציעה אפילו קצרה יותר לעיבוד מחרוזות של תאריכים ושעות. בנוסף, ישנן ספריות צד שלישי כמו Boost.Date_Time שמציעות פונקציונאליות דומה.

## ראה גם:
- מדריך לספריית `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- מדריך לפונקציית `put_time`: https://en.cppreference.com/w/cpp/io/manip/put_time
- הצצה לספריית `<fmt>` ל-C++20: https://fmt.dev/latest/index.html
- מידע נוסף על Boost.Date_Time: https://www.boost.org/doc/libs/1_78_0/doc/html/date_time.html
