---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
**Regular Expressions** (ביטויים רגולריים) מאפשרים לך לחפש תבניות בטקסט. הם כלי חזק בכל ידי משימות כמו ולידציה, פילוח, והחלפת מחרוזות. מתכנתים משתמשים בהם כי הם יעילים וחסכוניים בזמן.

## איך לעשות:
ב-C++ אנו משתמשים בספריית `<regex>` לעבודה עם ביטויים רגולריים:

```C++
#include <iostream>
#include <regex>

int main() {
  std::string text = "Hello 123, easy as 456, ABC.";
  std::regex number_regex("(\\d+)");
  
  std::smatch matches;
  while (std::regex_search(text, matches, number_regex)) {
    std::cout << "Number found: " << matches[0] << '\n';
    text = matches.suffix().str();
  }
  
  return 0;
}
```

פלט:
```
Number found: 123
Number found: 456
```

## צלילה עמוקה:
**היסטוריה**: ביטויים רגולריים שורשיהם בתורת האוטומטים ובלשנות. הם עברו תהליך של התפתחות, והיום ישנן שפות תכנות רבות המשתמשות בהם.

**אלטרנטיבות**: במקרים פשוטים, ניתן להשתמש בחיפוש רגיל או פונקציות כמו `find()` או `substr()`.

**פרטי יישום**: C++ תומכת בביטויים רגולריים דרך שימוש בספריית ה-`<regex>`. זה כולל מחלקות ופונקציות למציאה, התאמה, וחלוקה של מחרוזות לפי תבניות.

## ראה גם:
- [cppreference.com - Regex library](https://en.cppreference.com/w/cpp/regex)
- [cplusplus.com - Regular Expressions in C++](http://www.cplusplus.com/reference/regex/)
- [Regular Expressions.info - Tutorial](https://www.regular-expressions.info/tutorial.html)
