---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "C++: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
להמיר מחרוזת לתווים קטנים הוא תהליך שבו נשנה את כל האותיות במחרוזת לאותיות קטנות. תהליך זה נהוג לשימוש על מנת להפוך את הטקסט לאחיד ולקלוע קוד, ומצוין במגוון רחב של מטרות תכנותיות.

## איך לעשות:
הנה כמה דוגמאות להמרת מחרוזת לתווים קטנים בשפת C++:

```C++
#include <iostream>
#include <string>
#include <cctype>

int main()
{
  std::string str = "HELLO WORLD";//המחרוזת להמרה
  for(auto& c : str) //עבור כל תו במחרוזת
    c = std::tolower(c); //שנה את התו לתו קטן
  std::cout << str << '\n'; //הדפס את המחרוזת המומרת
  return 0;
}
```

פלט:
```
hello world
```

כמו כן, ניתן גם להשתמש בפונקציה `tolower` להמרת תו יחיד, כך:

```C++
#include <iostream>
#include <string>
#include <cctype>

int main()
{
  char c = 'A'; //התו להמרה
  c = std::tolower(c); //שנה את התו לתו קטן
  std::cout << c << '\n'; //הדפס את התו הממור
  return 0;
}
```

פלט:
```
a
```

## חקר עמוק:
העברת מחרוזת לתווים קטנים לא תמיד הייתה שגרתית בשפת C++. בעבר, השימוש בפונקציה `std::tolower` היה תלוי באנגלית בלבד, ולכן לא הייתה דרך יעילה להמיר מחרוזות לתווים קטנים בשפות אחרות. אולם, ממחקרים שנערכו במהלך השנים, נוצרו פונקציות תחלופה כמו `std::to_lower` ו- `std::tolower_l` שתומכות במגוון שפות ותרבויות.

לפני ביצוע ההמרה, חשוב לוודא שהמחרוזת מכילה אותיות בלבד. לכן, נוכל להשתמש בפונקציה `isalpha` (מהמילה "אלפא" באנגלית) לבדיקת כל תו במחרוזת והחלטה האם להפעיל את הפונקציה `tolower` עליו או להשאיר אותו כמו שהוא.

כמו כן, בשפת C++ קיימת דרך נוספת להמרת מחרוזת לתווים גדולים לתווים קטנים בעזרת הספריית מערכת, באמצעות הפונקציה `transform`:

```C++
#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

int main()
{
  std::string str = "Hello World";//המחרוזת להמרה
  std::transform(str.begin(), str.end(), str.begin(),
                 [](unsigned char c){ return std::tolower(c); });//המרת המחרוזת לתווים קטנים
  std::cout << str << '\n';//הדפס את המחרוזת המומרת
  return 0;
}
```

ניתן לראות שהשימוש ב- `transform` הוא מספר פעמים יותר אפקטיבי, ויכול להיות שימושי יותר עבור מחרוזות באורך גדול.

## ראה גם:
[מדריך למחרוזות בשפת C++](https://www.geeksforgeeks.org/stdstring-class-in-c/) - מכיל מידע על מגוון פונקציות ומתודות לעיבוד מחרוזות בשפת C++.

[C++ מדריך להפעל