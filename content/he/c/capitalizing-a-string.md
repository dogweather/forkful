---
title:                "הפיכת מחרוזת לאותיות ראשונות גדולות"
html_title:           "C: הפיכת מחרוזת לאותיות ראשונות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות ראשונות גדולות"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות גדולות היא פעולה שבה הופכים את האותיות הקטנות במחרוזת לאותיות גדולות. מתכנתים עשויים להשתמש בפעולה זו לשימושים שונים, כולל ניתוח של נתונים, ׸ידוא של קלט, ויצירת ממשק משתמש מגויס.


## איך לעשות:
נספר איך להפוך מחרודת לאותיות גדולות בשפת התכנות C:

```C
#include <ctype.h>
#include <stdio.h>
#include <string.h>

void to_upper(char* str) {
    for(int i = 0; str[i]; i++){
        str[i] = toupper(str[i]);
    }
}

int main() {
    char str[] = "hebrew programming";
    to_upper(str);
    printf("%s\n", str);
}

```
פלט מדגם:

```C
HEBREW PROGRAMMING
```

## צלילה עמוקה:
הפיכת מחרוזת לאותיות גדולות היא פעולה לא אופציונלית בכלל השפות.ב- ANSI C, הם כללו את `toupper` כחלק מ- `ctype.h`, אבל חלופות נוספות עשויות לשרת צרכים שונים בשפות תכנות אחרות.

אם תרצו לנסות גרסה בראשי תיבות (על-פי ASCII), אתם יכולים לעשות את השינוי הפשוט הבא:

```C
str[i] = str[i] & 0xDF;
```

## ראה עוד:
- [מדריך תכנות בC](https://www.learn-c.org)
- [תיעוד `toupper`](https://www.cplusplus.com/reference/cctype/toupper/)
- [תיעוד שפת C](http://www.open-std.org/jtc1/sc22/wg14/)