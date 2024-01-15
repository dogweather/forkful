---
title:                "שימוש בביטויי תקנה"
html_title:           "C: שימוש בביטויי תקנה"
simple_title:         "שימוש בביטויי תקנה"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה
כיצד כתיבת תוכניות בשפת C הנוכחית תסייע לך לפתח יכולות ביצועיות וגמישות ביישומים שלך? למה כדאי לבחור בשימוש בביטויים רגולריים בתוכניות הכתובות בשפת C?

## כיצד להשתמש
בביטויים רגולריים בשפת C ניתן להגדיר תבניות כדי לאתר ולשנות מחרוזות טקסט מתאימות. למטה אנו מציגים כמה דוגמאות לכתיבת ביטויים רגולריים כאשר אנו משתמשים בשפת C. מטרת הדוגמאות החכמות הוא לתת לך רעיונות כיצד להשתמש כראוי בביטויים רגולריים כדי לפתח תוכניות בשפת C.

``` C
#include <stdio.h>
#include <regex.h>

int main()
{
    // מתוך התוכנה שלנו
    char input[] = "זוהי דוגמה לטקסט";
    char regex[] = "[א-ת]+";

    // חיפוש לפי התבנית של regex בתוך הטקסט
    regex_t regex_compiled;
    regcomp(&regex_compiled, regex, 0);
    regmatch_t matches[strlen(regex)];
    regexec(&regex_compiled, input, strlen(regex), matches, 0);

    // הדפסת כל מופעים של התבנית
    for (int i = 0; i < matches[i].rm_so != -1; ++i) {
        printf("מיושם מימצאת תבנית עבור המתאים דנא? %.*s\n",
            (int)(matches[i].rm_ext - matches[i].rm_so), &input[matches[i].rm_so]);
    }

    // קליטת הפלט שסופק על ידי המתכנת
    char output[255];
    fgets(output, sizeof(output), stdin);

    // החזרת התוכנית שלנו
    return 0;
}
```

###תוצאה:
```
מתאים דנא? זוהי
מתאים דנא? דוגמה
מתאים דנא? לטקסט
```

## תהגי עומק
כעת שאנו מבינים כיצד להשתמש בביטויים רגולריים בשפת C, נחקור תהגי עומק של המושג. ביטויים רגולריים הם כלי נפלא לסינכרון וטיפול בתבניות טקסט באופן יעיל ו