---
title:    "C: שימוש בכיתובי משנה במחשבים"
keywords: ["C"]
---

{{< edit_this_page >}}

# למה

Extraction of substrings היא תהליך חיוני בשפת תכנות C המאפשר למפתחים לחלץ תתי מחרוזות ממחרוזות גדולות יותר. ניתן להשתמש בפונקציות של מילולי של C להפיק חלקים מנתונים וליצור מחרוזות חדשות בעלות ערך ופורמט שונים.

# איך לעשות זאת

הנה דוגמאות של קוד תכנותי בשפת C עבור חלוץ תתי מחרוזות:

```c
// חיתוך תת מחרוזת ממיקום מסוים עד סוף המחרוזת
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "פורץ שמיים";
    char substring[7];
    int starting_index = 6;

    strncpy(substring, str + starting_index, strlen(str) - starting_index);
    printf("תת המחרוזת הוא: %s\n", substring);

    return 0;
}
```

פלט:

```
תת המחרוזת הוא: שמיים
```

```c
// חיתוך תת מחרוזת מהתחלה עד מיקום מסוים
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "מפבר חורף";
    char substring[5];
    int ending_index = 4;

    strncpy(substring, str, ending_index + 1);
    printf("תת המחרוזת הוא: %s\n", substring);

    return 0;
}
```

פלט:

```
תת המחרוזת הוא: מפבר
```

# לחפור עמוק יותר

לחיצה תת מחרוזות היא תהליך פשוט אך חשוב בתכנות עם שפת C. קיימים מספר פונקציות שמגדירות ומפעילות את תהליך חיתוך תת מחרוזות, כגון `strncpy()`, `strchr()`, ו-`strtok()`. לפונקציות אלו ישנן כמה אפשרויות להצבת אותיות מסוימות, סימנים או תווים ליצירת תתי מחרוזות. כמו כן, ניתן לשלב את הפונקציות הללו כדי להגיע לתוצאה הרצויה.

# ראה גם

- [תיעוד C על חיתוך תת מחרוזות](https://www.tutorialspoint.com/c_standard_library/c_function_strtok.htm)
- [תיעוד C על פונקציות מילולי](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)
- [