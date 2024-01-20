---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

אינטרפולציה של מחרוזת היא הכנסת משתנים ישירות לתוך מחרוזת. מתכנתים עושים זאת כדי לחסוך בדיכאון גבוה של מחרוזות ולהפוך את הקוד ליותר קריא ונוח לכתיבה.

## איך לעשות:

נשתמש בשפת C++20 בו הוצגה תכונה חדשה שמאפשרת אינטרפולציה של מחרוזת באמצעות מחרוזות מעוצבות. נניח, לדוגמה, בשם דוגמאית ובמספר של כדורים.

```C++
#include <string>
#include <format>

int main() {
    std::string name = "Yosi";
    int balls = 5;

    std::string message = std::format("Hello {}, you have {} balls.", name, balls);
    std::cout << message << std::endl;
    return 0;
}
```
הפלט של הקוד הוא: `Hello Yosi, you have 5 balls.`

## הסבר מעמיק

אינטרפולציה של מחרוזת התחילה להינות מודעות רחבה בשנת 1980 עם שפת Perl, ומאז נפוצה בשפות כמו Python, Ruby ו- JavaScript. לפני שהוצעה התכונה החדשה ב- C++20, אפשר היה להשתמש בספריות צד שלישי כמו fmt או boost. 

אם אתה לא משתמש ב-C++20 עליך שרשרת `to_string` ו- `+` למרכיבים שונים של המחרוזת או להשתמש בספריות צד שלישי.

```C++
std::string name = "Yosi";
int balls = 5;
std::string message = "Hello " + name + ", you have " + std::to_string(balls) + " balls.";
std::cout << message << std::endl;
```
הפלט של הקוד הוא: `Hello Yosi, you have 5 balls.`

## לקריאה נוספת:

זה רק סקיצה של תכונה כה רחבה וחדשה בשפת התכנות שלנו ואני ממליץ לך לשקול להעמיק:
- שפת מסמך C++20 עצמה: https://isocpp.org/std/the-standard
- ספרייה FMT: https://fmt.dev/latest/index.html
- ספריית Boost Format: https://www.boost.org/doc/libs/1_76_0/libs/format/doc/format.html