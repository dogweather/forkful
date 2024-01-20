---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
חיפוש אורך של מחרוזת הוא הבנה של כמה תווים קיימים בה. מתכנתים אופטים להשתמש בזה כדי למנוע שגיאות שמקורן באורך המחרוזת ולייעל תהליכים.

## איך עושים זאת:
בהלך הפעולה, אתה מגדיר מחרוזת ומשתמש בפונקציה `length ()` כדי למצוא את אורך המחרוזת. הנה דוגמה:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string my_string = "Hello world!";
    cout << "Length of string is: " << my_string.length();
    return 0;
}
```

פלט:

```
Length of string is: 12
```

## צלילה מעמיקה:
**ההיסטוריה:** בC, לפני שהוכנסה שפת C++, מתכנתים היו נאלצים לחשב "אורך" של מחרוזת באמצעות שפוך את המחרוזת עם לולאה while עד שהם יגיעו לסוף התו הסיומת של מחרוזת.

**אלטרנטיבות:** למרות שתיכנים את `length ()`, אפשר גם להשתמש ב `size ()`, שמחזיר את אותו הערך. השימוש בכל אחד מהם תלוי בהעדפה אישית.

**פרטי יישום:** הפונקציה `length ()` ו `size ()` מחזירות את אורך המחרוזת כמספר שלם אי-שלילי המציין את מספר התווים שבמחרוזת.

## ראה גם:
- [חומר עזר על פונקציות מחרוזת של C++](https://en.cppreference.com/w/cpp/string/basic_string)
- [C++ String פונקציות](http://www.cplusplus.com/reference/string/string/)