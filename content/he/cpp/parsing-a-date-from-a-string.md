---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
פענוח תאריך ממחרוזת באופן תלחיתי הוא תהליך של המרת מחרוזת טקסט לתאריך, בהנחה שהוא כתוב בפורמט מסוים. מתכנתים משתמשים בפעולה זו למניעת שגיאות קלט ולהכוונת המשתמש לקלט חוקי.

## איך לעשות:
נקודת ההתחלה שלנו היא מחרוזת תאריך בפורמט "YYYY-MM-DD". היש דרך להמיר אותה לסוג התאריך של C++ באמצעות הספרייה `<chrono>`.

```C++
#include <chrono>
#include <sstream>

std::istringstream ss("2023-01-17");
std::chrono::year_month_day ymd;
ss >> std::chrono::parse("%F", ymd);
// now ymd holds the date 2023-Jan-17
```

## בהעמקה
ההיסטוריה של פענוח התאריכים ממחרוזות ב-C++ פחות מפורשת מאשר בשפות תכנות אחרות, כמו ג'אווה או Python. נערך הרבה אינטרפרטציה לפני הגעה לתקן C++20, שהציג את `<chrono>`, ספרייה חדשה לעיבוד הזמן.

לראשונה, `std::istringstream` משמשת לקריאה ממחרוזת, והפתרון `std::chrono::parse` מרחיב זאת למחרוזות תאריך.

דרך חלופית לביצוע זה הייתה שימוש ב-`std::strftime`, אך הוא מחייב התמודדות מרובה עם C-strings, שהן מסורבלות יותר.

מי שרוצה להשתמש ב-`<chrono>` צריך להתמודד עם מושגים חדשים כמו `std::chrono::year_month_day`. זה גורם להכרח לקרוא יותר, אך בסופו של דבר הקוד נהיה יעיל ונקי יותר.

## קישורים למקורות נוספים
העמקה:
- [הפענוח של מחרוזות תאריך וזמן](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time/posix_time.html)

קודדים וקודים אחרים:
- [המרת מחרוזת לתאריך](https://stackoverflow.com/questions/10716042/fastest-way-to-convert-stl-string-to-date-and-hour)
- [הפענוח של מחרוזות תאריך וזמן](https://www.boost.org/doc/libs/1_52_0/doc/html/date_time/date_time_io.html)