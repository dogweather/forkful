---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
simple_title:         "הפיכת מחרוזת לאותיות רישיות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות רישיות היא התהליך שבו אתה משנה את כל אותיות המחרוזת לאותיות גדולות. תוכניתנים עושים זאת לשם עקביות, קריאות, או לתפוקה שדורשת את זה, כמו כותרות או קודים.

## איך לעשות:
קטע קוד ב-C להמחשה:

```C
#include <stdio.h>
#include <ctype.h>

void capitalize(char *str) {
    while (*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "shalom, world!";
    capitalize(myString);
    printf("%s\n", myString);    // Output: SHALOM, WORLD!
    return 0;
}
```

כאשר אתה מריץ את הקוד הזה, הפלט יהיה:
```
SHALOM, WORLD!
```

## עיון נוסף:
בחלוף השנים, שפות תכנות התפתחו עם פונקציות פנימיות למניפולציות מחרוזת. ב-C, אתה צריך לעשות את זה בעצמך, כמודגם. דוגמאות אלטרנטיביות כוללות שימוש ב-fgetc ו-fputc לקריאה וכתיבה אות לאות, או בשפות גבוהות יותר כמו Python עם .upper(). הכי חשוב לזכור שהפונקציה toupper מצריכה קלט מסוג unsigned char לקידוד נכון של האותיות.

## ראה גם:
- התיעוד הרשמי של C לפונקציה `toupper`: https://en.cppreference.com/w/c/string/byte/toupper
- מדריך לפונקציות למחרוזות ב-C: https://www.tutorialspoint.com/c_standard_library/string_h.htm
- תיעוד שפת C: http://www.iso-9899.info/wiki/The_Standard
