---
title:                "C++: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

יש להסיר תווים שתואמים תבנית מתוך טקסט כאשר מתווכים לשם קוד פשוט יכול לעזור במגוון דברים, כגון מיון וסינון נתונים ופתרון בעיות בפורמטים תקינים. למרבה המזל, C++ מספק כמה עתרים יעילים להסרת תווים תואמים תבנית מטקסט.

## כיצד לעשות זאת

מישהו במקום הקורא יכול להשתמש בפעולות כגון `erase()` עם איטרטורים כדי להסיר תווים תואמים תבנית מהטקסט. ננסה להסיר כל האותיות הגדולות מטקסט מסוים, "Hello World!" לדוגמה.

```C++
string text = "Hello World!";

text.erase(remove_if(text.begin(), text.end(), ::isupper), text.end());

cout << text << endl;
```

הפלט יהיה:

```
ello orld!
```

כאן אנו משתמשים בפונקצית `remove_if` כדי להעביר את כל האותיות הגדולות כארגומנט. לאחר מכן, תת פונקציה `erase` משתמשת במיקום הכתובת של המצוין למחוק את התווים התואמים.

## העמקת הנושא

אם אתה מתעניין בפרטי ניתוח ועיבוד טקסט יותר, אתה יכול לחפש פקודות נוספות כמו `regex_replace` ו- `regex_search` שמאפשרות לנו להתמודד עם תבניות מורכבות יותר. ניתן גם להשתמש בביטויים רגולריים ולהסיר תווים בהתאמה לתבניות מסוימות מטקסט. כמו כן, ישנם עתרים שונים זמינים עבור שפות תכנות שונות, כמו Python ו- Java.

## ראה גם

- [C++ Reference: `remove_if` Function](https://www.cplusplus.com/reference/algorithm/remove_if/)
- [C++ Regex Tutorial](https://www.cplusplus.com/articles/regex/)
- [RegexOne - Learn Regular Expressions with Simple, Interactive Exercises](https://regexone.com/)