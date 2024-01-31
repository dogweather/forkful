---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:55:41.344344-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? מה ולמה?
קריאת ארגומנטים משורת הפקודה היא לקחת נתונים שהמשתמש מזין לתוכנה כשהוא מריץ אותה. זה חשוב כי זה מאפשר התאמה אישית וגמישות בהרצת תוכניות.

## How to: איך לעשות:
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("There are %d arguments provided:\n", argc - 1);
  for (int i = 1; i < argc; ++i) {
    printf("Argument %d: %s\n", i, argv[i]);
  }
  return 0;
}
```

רץ את הקוד עם: `./program_name arg1 arg2 arg3`
תקבל פלט:
```
There are 3 arguments provided:
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

## Deep Dive: עומק של ידיעה
בשנות ה-70, כש-C עוד הייתה חדשה, חוקרים ב-Bell Labs פיתחו את הקונבנציה של ארגומנטים בשורת הפקודה כדי לאפשר גיוון יותר גדול בשימוש בתכנות הם כתבו. אתה יכול לקרוא גם על `getopt()` לקבלת פרמטרים. עוד אופציה היא להשתמש בספריות חיצוניות כמו `argp` לקולות שורת הפקודה שלך. כאשר קוראים ארגומנטים, נהוג לבדוק תקפותם ולהציג הודעות עזר ושגיאה כאשר נדרש.

## See Also: ראה גם:
- GNU C Library: https://www.gnu.org/software/libc/manual/html_node/Parsing-Program-Arguments.html
- `getopt` manual: https://man7.org/linux/man-pages/man3/getopt.3.html
- Tutorial on Command Line Arguments in C: https://www.cprogramming.com/tutorial/c/lesson14.html
