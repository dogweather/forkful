---
title:                "C: עובדים עם ביטויים רגולריים"
simple_title:         "עובדים עם ביטויים רגולריים"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

תכנות ב-C הוא משימה די מסובכת, אבל ייתכן שתתקלו פעם בשימוש בביטויים רגולריים. הם הם כלים אדירים שיכולים לסייע לכם למצוא ולטפל בטקסט בצורה מהירה ויעילה. בכתיבת דפי אינטרנט, תרגום ואפילו תוכניות מקוונות, הביטויים הם כלי עוצמתי עוזר להתמודדות עם כמויות גדולות של טקסט.

## איך לבצע קוד

ראשית, נצטרך להכיר את הביטויים הרגולריים המבניים. ב-C, ניתן להשתמש בפונקציית "regex.h" כדי לכתוב ביטויים רגולריים בתוך הקוד שלנו. ניתן להשתמש בפונקציות כגון "regex_compile()" ו "regex_match()" כדי לעבוד עם הביטויים שלנו ולבדוק אם הם מתאימים לטקסט ספציפי.

הנה דוגמא לקוד ב-C שבו אנו משתמשים בביטוי רגולרי לאתר רשומות אימייל בתוך טקסט מסוים:

```C
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main() {
    regex_t regex;
    int reti;
    char text[100] = "האימייל שלי הוא example@example.com";
    char pattern[100] = "[a-zA-Z0-9]+@[a-zA-Z]+.[a-zA-Z]+";

    reti = regcomp(&regex, pattern, 0);
    if (reti) {
        fprintf(stderr, "בעיה בשילוב ביטוי רגולרי");
        exit(1);
    }

    // מתודת regexec מחזירה 0 אם המשתנה הזה מתאים לטקסט, אחרת המשתנה מוציא את מיקום התאמה הראשון
    reti = regexec(&regex, text, 0, NULL, 0);
    if (!reti) {
        printf("האימייל שנמצא הוא: %s\n", text);
    }
    else if (reti == REG_NOMATCH) {
        printf("לא נמצאו אימיילים\n");
    }
    
    regfree(&regex);

    return 0;
}
```

הפלט של הקוד הזה יהיה:

```
האימייל שנמצא הוא: example@example.com
```

כפי שאתם רואים, הביטוי החוקי של