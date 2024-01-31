---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:38:30.407049-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה? (What & Why?)
המרת מחרוזת לאותיות קטנות זהו תהליך שבו כל אותיות גדולות במחרוזת הופכות לקטנות. תכניתנים עושים זאת לשם עקביות, השוואות קלות יותר, ועיבוד טקסט.

## איך לעשות: (How to:)
קוד C פשוט להמרת מחרוזת לאותיות קטנות:

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "Shalom, Olam!";
    toLowerCase(myString);
    printf("%s\n", myString); // ידפיס "shalom, olam!"
    return 0;
}
```

## עיון מעמיק: (Deep Dive)
המרת מחרוזת לאותיות קטנות היא שיטה נפוצה שהייתה קיימת מאז הימים הראשונים של מעבדי טקסט. קיימות שיטות חלופיות כמו שימוש בפונקציות מובנות בשפות תכנות מודרניות, אבל ב-C אנחנו משתמשים ב-loop וב-func `tolower()` מהספריה `<ctype.h>`. יש לשים לב לשימוש ב-(unsigned char) על מנת למנוע שגיאות במידה ומשתמשים באופיינים שתוכנתם אינן ASCII.

## ראו גם: (See Also)
- [ASCII Table and Description](http://www.asciitable.com/)
- [GNU C Library - Character Handling](https://www.gnu.org/software/libc/manual/html_node/Character-Handling.html)
