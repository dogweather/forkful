---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

חיפוש והחלפה של טקסט הוא פעולת עיבוד מידע שלמה, שבה אנו מחפשים מחרוזת מסוימת בקטע טקסט ומחליפים אותה במחרוזת אחרת. מתכנתים נדרשים לבצע פעולות אלה כדי לעבד נתונים, לשפר את התאמת הטקסט ולשפר את הקריאות.

## איך לעשות את זה: 

קוד בסיסי של חיפוש והחלפה של מחרוזת ב-C:

```C
#include <string.h>
#include <stdio.h>

void search_and_replace(char* str, char* find, char* replace){
    char buffer[1024];
    char *p;

    p = strstr(str, find);
    strncpy(buffer, str, p-str); 
    buffer[p-str] = '\0';

    sprintf(buffer+(p-str), "%s%s", replace, p+strlen(find));
    
    printf("%s\n", buffer);
}

int main() {
    char str[] = "Hello World!";
	char find[] = "World";
	char replace[] = "Programmers!";
	search_and_replace(str, find, replace);
    return 0;
}
```

הפלט:

```C
"Hello Programmers!"
```

## Deep Dive:

1. **היסטוריה:** חיפוש והחלפה של מחרוזות הוא מרכיב מרכזי בעיבוד טקסט מאז ימי המחשבים הראשונים. הוא ממומש במערכות הפעלה, שפות תכנות, וכחלק מאפליקציות תוכנה.
 
2. **אלטרנטיבות:** ניתן להוסיף חלופות יותר מורכבות, כמו חיפוש של ביטויים רגולריים, תוך שימוש בשיטות שהוזכרו למעלה.
  
3. **פרטי ביצוע:** מנהלי הזיכרון הם מנגנונים מרכזיים בהחלפה של מחרוזות בשפת C. חשוב לנהל את המחסנים כראוי כדי למנוע עיוותים ושגיאות בזמן הריצה.

## ראה גם: 

2. [פונקציות של מנהל זיכרון ב-C](http://www.cplusplus.com/reference/cstring/)