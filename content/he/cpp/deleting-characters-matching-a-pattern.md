---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "C++: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שמכילים התאמה לתבנית היא פעולה שמאפשרת למתכנתים להסיר תווים ספציפיים מתוך מחרוזות. כך ניתן לייצר מחרוזות חדשות או לעבוד עם נתונים ממוזגים ללא תווים מיותרים. מתכנתים מבצעים פעולות מחיקה כדי לשמור על נקיון קוד ולהפחית מספר התווים במחרוזת.

## איך ל:
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // פריצת מצוקה למחרוזת עם תווים מיותרים
    string input = "H-e-l-l-o'-'-W-o-r-l-d-'-'-'-'-'-'-'-'-";
    string output;

    // חישוב אורך המחרוזת המקורית
    int len = input.length();

    // לולאת פור עבור כל תו ובדיקה האם היא תו היפוך
    for (int i = 0; i < len; i++)
    {
        if (input[i] != '-')
        {
            output = output + input[i];
        }
    }

    // פלט תוכן המחרוזת המעודכןה
    cout << output << endl;;

    return 0;
}
```
output: Hello World

## מעמקים:
מחיקת תווים שמכילים התאמה לתבנית היא פעולה שנתקיימת כבר מתקופת התכנות הראשונה. פעולה זו נכתבת בכדי לפנות מחרוזות מיותרות ולתת למתכנתים כלי יעיל לעיבוד נתונים. כיום ישנם גם כלים נוספים להשתמש כדי למחוק מחרוזות, כמו פעולות פריצת מצוקה, ושימוש בתורת ביטויים רגולריים.

## ראו גם:
- [תיעוד רשמי של C++](https://isocpp.org/)
- [מדריכים ושאלונים בנושא תורת ביטויים רגולריים](http://www.cplusplus.com/reference/regex/)
- [פוסטים נוספים על תכנות בסגנון זה בבלוג](https://cpptruths.blogspot.com/)