---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
simple_title:         "שימוש בביטויים רגולריים"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
Regular expressions, רגקס בקיצור, הן כלים לחיפוש ועיבוד טקסט באמצעות פטרנים. תוכניתנים משתמשים בהם כי הם מאפשרים חיפוש מהיר וחכם של מחרוזות, תיקונים מורכבים, ואימות קלט.

## איך לעשות:
```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int result;
    result = regcomp(&regex, "^a[[:alnum:]]", 0); // קומפילציה של רגקס לחיפוש מילים המתחילות ב-'a'
    if (result) { 
        printf("הרגקס לא חוקי.\n");
        return 1;
    }
  
    result = regexec(&regex, "apple", 0, NULL, 0);
    if (!result) {
        printf("המילה מתאימה!\n");
    } else if (result == REG_NOMATCH) {
        printf("אין התאמה.\n");
    } else {
        printf("שגיאת חיפוש.\n");
    }

    regfree(&regex); // ניקוי הרגקס
    return 0;
}
```
פלט דוגמה:
```
המילה מתאימה!
```

## צלילה לעומק
השימוש ברגקסים סביב מאז השנות ה-50 והתפתח במרוצת השנים. יש אלטרנטיבות כמו חיפוש טקסט קלאסי, אך הם פחות גמישים. ב-C, רגקסים ממומשים דרך הספרייה regex.h, לעיתים קרובות עובדים ביחד עם מחרוזות סטנדרטיות בממשק POSIX.

## ראה גם
- [GNU C Library Manual – Regular Expressions](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [regex(7) - Linux man page](https://man7.org/linux/man-pages/man7/regex.7.html)
- [POSIX Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression#POSIX_basic_and_extended)
