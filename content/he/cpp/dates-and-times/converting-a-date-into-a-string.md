---
date: 2024-01-20 17:36:58.828984-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-C++, \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05EA `<chrono>` \u05DC\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D5\u05D6\u05DE\u05E0\u05D9\u05DD \u05D5\u05D1\
  \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA `<iomanip>` \u05DC\u05D4\u05DE\u05E8\u05EA\u05DD\
  \ \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA."
lastmod: '2024-03-13T22:44:39.855205-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-C++, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9\
  \ \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA `<chrono>` \u05DC\u05D8\u05D9\u05E4\
  \u05D5\u05DC \u05D1\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D5\u05D6\u05DE\
  \u05E0\u05D9\u05DD \u05D5\u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA `<iomanip>`\
  \ \u05DC\u05D4\u05DE\u05E8\u05EA\u05DD \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

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
